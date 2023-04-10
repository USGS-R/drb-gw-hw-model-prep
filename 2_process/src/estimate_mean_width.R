#' Function to estimate mean width for an NHDPlusv2 reach
#'
#' @param nhd_lines sf linestring containing the NHDPlusv2 flowlines. Must
#' contain column COMID indicating the unique reach identifier
#' @param buffer_dist_m integer; look for NWIS sites within this distance
#' in meters. Defaults to 500 m.
#' @param estimation_method character string; what method should be used 
#' to estimate NHDflowline widths? valid options include "nwis" and "raymond2012".
#' @param network_pos_variable character string; if estimation_method is 'nwis',
#' what NHDPlusv2 attribute should be used to build an empirical regression that
#' predicts mean width? Valid entries are "arbolate_sum", "upstream_area" and "ma_flow", 
#' which is the gage-adjusted mean annual flow for a given COMID as indicated by the 
#' value-added attribute QE_MA.
#' @param ref_gages sf object representing the ref-gages dataset, downloaded from
#' https://github.com/internetofwater/ref_gages. ref_gages should contain the
#' columns provider_id, name, provider, and COMID_refgages. Defaults to NULL,
#' however, estimation_method = 'nwis' requires ref_gages as an input.
#' 
estimate_mean_width <- function(nhd_lines, buffer_dist_m = 500, estimation_method, 
                                network_pos_variable, ref_gages = NULL){
  
  # Check that user entries match expected inputs
  stopifnot("Value provided for estimation_method is not a valid entry" =
              estimation_method %in% c("nwis"))
  stopifnot("Value provided for network_pos_variable is not a valid entry" =
              network_pos_variable %in% c("arbolate_sum","upstream_area","ma_flow"))
  stopifnot("estimation_method = 'nwis' requires ref_gages as an input" = 
              estimation_method == "nwis" & !is.null(ref_gages))
  
  # 1) One approach is to estimate width using an empirical relationship
  # between measured width and upstream area that is developed using
  # USGS field measurements of width. 
  if(estimation_method == 'nwis'){
    
    message("Downloading width data from NWIS...")
    
    # return a list of nwis sites within the bounding box of nhd_lines
    bbox <- sf::st_bbox(nhd_lines)
    nwis_sites <- dataRetrieval::whatNWISsites(bBox = bbox,
                                               parameterCd = "00060")
    
    # Format nwis sites and retain those that are stream sites
    nwis_sites_sf <- nwis_sites %>% 
      sf::st_as_sf(coords = c('dec_long_va','dec_lat_va'), crs = 4269) %>%
      sf::st_transform(st_crs(nhd_lines)) %>%
      filter(site_tp_cd %in% c("ST","ST-TS"))
    
    # The bounding box approach above returns more sites than we're
    # interested in, omit sites that aren't within some distance of
    # the given nhd network 
    nwis_sites_along_nhd <- nwis_sites_sf %>%
      sf::st_filter(y = nhd_lines, 
                    .predicate = st_is_within_distance,
                    dist = units::set_units(buffer_dist_m, m))

    # Join nwis sites to nearest nhd flowline 
    nwis_sites_nearest_nhd <- nwis_sites_along_nhd %>%
      sf::st_transform(st_crs(nhd_lines)) %>%
      sf::st_join(y = nhd_lines, join = st_nearest_feature) %>%
      mutate(COMID_nearest = as.character(comid)) %>%
      select(c(agency_cd:queryTime, COMID_nearest))
    
    # Compare nearest flowline with value in ref-gages. If site is 
    # included in ref-gages, defer to ref-gages COMID assignment. 
    # Otherwise, use nearest COMID identified in previous step. 
    nwis_sites_snapped <- nwis_sites_nearest_nhd %>%
      left_join(y = ref_gages %>%
                  mutate(site_no = stringr::str_replace(provider_id, "USGS-","")) %>%
                  sf::st_drop_geometry() %>%
                  select(site_no, COMID_refgages),
                by = c("site_no")) %>%
      mutate(COMID = if_else(!is.na(COMID_refgages), COMID_refgages, COMID_nearest)) %>%
      select(c(agency_cd:queryTime, COMID))
    
    # Join snapped NWIS sites to NHD attributes using COMID 
    # identified in previous step.
    nwis_sites_w_attr <- nwis_sites_snapped %>%
      left_join(y = nhd_lines %>%
                  sf::st_drop_geometry() %>%
                  mutate(COMID = as.character(comid)) %>%
                  select(COMID, gnis_name, arbolatesu, totdasqkm, lengthkm, qe_ma, 
                         streamorde, streamcalc),
                by = "COMID") %>%
      # only retain sites snapped to dendritic river segments since
      # divergent reaches may have smaller/larger widths than
      # would be expected by their upstream length or area
      filter(streamorde == streamcalc)
    
    # Download width measurements from NWIS
    nwis_widths <- nwis_sites_w_attr %>%
      sf::st_drop_geometry() %>%
      split(.,.$site_no) %>%
      lapply(., function(x){
        tryCatch(summarize_nwis_widths(x$site_no), 
                 error = function(cond){
                   return(NULL)
                 },
                 warning = function(cond){
                   return(NULL)
                 })
      }) %>%
      bind_rows()
    
    # create empirical relationship between NWIS widths and arbolate sum, 
    # as in this USGS report, https://pubs.usgs.gov/sir/2021/5116/sir20215116.pdf,
    # or by total upstream area, or by the gage-adjusted estimated mean annual 
    # discharge (NHDPlus attribute qe_ma)
    message(sprintf(
      paste0("Estimating width for NHDPlusv2 flowlines based on empirical fit ",
             "between NWIS measurements and attribute %s. The range of widths ",
             "represented in this relationship is %s - %s meters"), 
      network_pos_variable, round(min(nwis_widths$width_m),1), round(max(nwis_widths$width_m),0)))
    
    nwis_sites_w_width <- nwis_widths %>%
      left_join(nwis_sites_w_attr, by = "site_no") %>%
      mutate(ma_flow_m3s = qe_ma * 0.0283168)
      
    # fit and summarize empirical relationships
    fit_arbsum <- summary(lm(log10(width_m) ~ log10(arbolatesu), data = nwis_sites_w_width))
    fit_totda <- summary(lm(log10(width_m) ~ log10(totdasqkm), data = nwis_sites_w_width))
    fit_qe_ma <- summary(lm(log10(width_m) ~ log10(ma_flow_m3s), data = nwis_sites_w_width))
    
    width_fits <- tibble(network_pos_variable = c('arbolate_sum','upstream_area','ma_flow'),
                         slope = c(fit_arbsum$coefficients[2],
                                   fit_totda$coefficients[2],
                                   fit_qe_ma$coefficients[2]),
                         intercept = c(fit_arbsum$coefficients[1],
                                       fit_totda$coefficients[1],
                                       fit_qe_ma$coefficients[1]),
                         r.squared = c(fit_arbsum$r.squared, 
                                       fit_totda$r.squared, 
                                       fit_qe_ma$r.squared))
    
    # apply regression coefficients to predict width across nhdv2 reaches
    # width = a * (arb_sum^b)
    if(network_pos_variable == "arbolate_sum"){
      a_coeff_arbsum <- 10^width_fits$intercept[width_fits$network_pos_variable == "arbolate_sum"]
      b_coeff_arbsum <- width_fits$slope[width_fits$network_pos_variable == "arbolate_sum"]
      
      nhd_lines_out <- nhd_lines %>%
        mutate(est_width_m = a_coeff_arbsum * (arbolatesu^b_coeff_arbsum))
    }
    if(network_pos_variable == "upstream_area"){
      a_coeff_totda <- 10^width_fits$intercept[width_fits$network_pos_variable == "upstream_area"]
      b_coeff_totda <- width_fits$slope[width_fits$network_pos_variable == "upstream_area"]
      
      nhd_lines_out <- nhd_lines %>%
        mutate(est_width_m = a_coeff_totda * (totdasqkm^b_coeff_totda))
    }
    if(network_pos_variable == "ma_flow"){
      a_coeff_qema <- 10^width_fits$intercept[width_fits$network_pos_variable == "ma_flow"]
      b_coeff_qema <- width_fits$slope[width_fits$network_pos_variable == "ma_flow"]
      
      nhd_lines_out <- nhd_lines %>%
        mutate(est_width_m = a_coeff_qema * ((qe_ma * 0.0283168)^b_coeff_qema))
    }
  }
  
  # 2) Another approach is to estimate width using the empirical relationship
  # between synthesized measured widths and discharge presented in Raymond et
  # al. 2012, https://doi.org/10.1215/21573689-1597669. This dataset likely
  # skews more toward smaller streams and rivers compared to NWIS measurements.
  if(estimation_method == 'raymond2012'){
    a_coeff_raymond <- 12.88
    b_coeff_raymond <- 0.42
    
    nhd_lines_out <- nhd_lines %>%
      mutate(est_width_m = a_coeff_raymond * ((qe_ma * 0.0283168)^b_coeff_raymond))
    
  }
  
  return(nhd_lines_out)

}


#' Function to download USGS in situ field measurements of river width
#' 
#' @param site_no character string indicating the USGS site number
#' 
#' @examples download_nwis_widths("01481000")
#' 
download_nwis_widths <- function(site_no){
  
  # download width measurements from NWIS
  width_data <- dataRetrieval::readNWISmeas(siteNumbers = site_no, expanded = TRUE)
    
  # Format in situ width measurements
  width_data_out <- width_data %>%
    filter(!is.na(chan_width)) %>%
    mutate(width_m = chan_width * 0.3048) 
  
  return(width_data_out)
  
}



#' Function to download USGS in situ field measurements of river width
#' and return the median width in meters.
#' 
#' @param site_no character string indicating the USGS site number
#' @param earliest_date character string formatted as "YYYY-MM-DD", will 
#' return width measurements made on or after this date. Defaults to NULL, 
#' which returns all available width measurements.
#' @param latest_date character string formatted as "YYYY-MM-DD", will 
#' return width measurements made on or before this date. Defaults to NULL,
#' which returns all available width measurements.
#' 
#' @examples summarize_nwis_widths("01481000", earliest_date = "1980-10-01")
#'
summarize_nwis_widths <- function(site_no, earliest_date = NULL, latest_date = NULL){
  
  # Download width data from NWIS
  width_data <- download_nwis_widths(site_no)
  
  # Summarize in situ field widths
  width_data_out <- width_data %>%
    # For days with multiple width measurements, summarize
    # data into a single width value for that date:
    group_by(measurement_dt) %>%
    summarize(site_no = unique(site_no),
              width_median_dt = median(width_m),
              .groups = 'drop') %>%
    # If `earliest_date` is not NULL, return only width measurements
    # made on or after `earliest_date`:
    {
      if(!is.null(earliest_date)){
        filter(.,measurement_dt >= earliest_date)
      } else {.}
    } %>%
    # If 'latest_date' is not NULL, return only width measurements
    # made on or before `latest_date`:
    {
      if(!is.null(latest_date)){
        filter(.,measurement_dt <= latest_date)
      } else {.}
    } %>%
    summarize(site_no = unique(site_no),
              width_m = median(width_median_dt),
              width_measures_n = n(),
              .groups = 'drop')
  
  return(width_data_out)
  
}

  



