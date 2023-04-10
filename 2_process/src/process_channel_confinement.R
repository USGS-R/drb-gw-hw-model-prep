#' @title Process McManamay channel confinement data
#' 
#' @description 
#' Function to process McManamay and DeRolph (2018) channel confinement dataset,
#' including to aggregate the values from NHDv2 reaches to NHM segments.
#' 
#' @param confinement_data data frame containing McManamay channel confinement
#' data. Must include columns "COMID", "VBL", "RL", "RWA", and "VBA".
#' @param nhd_nhm_xwalk data frame that specifies how NHDPlusv2 COMIDs map
#' onto NHM segment identifiers. Must contain columns "COMID", "PRMS_segid",
#' and "seg_id_nat".
#' @param force_min_width_m numeric value that indicates what the minimum 
#' width value used in channel confinement calculations should be. All width
#' values below this minimum width will be set to this value. Optional;
#' defaults to zero, which amounts to this setting being ignored.
#' @param preferred_width_df Optional; data frame containing preferred width
#' values to use when calculating channel confinement for NHDPlusv2 reaches.
#' Must contain columns "COMID" and "width_m". Defaults to NULL, in which 
#' case width estimates from McManamay and DeRolph (2018) will be used to
#' estimate channel confinement. 
#' @param network character string indicating the requested resolution of the
#' channel confinement values. Options include "nhdv2" or "nhm". If "nhm", 
#' the river and valley bottom lengths and areas will be aggregated to NHM
#' flowlines and the channel confinement categories will be assigned using
#' the same criteria as described in the McManamay and DeRolph (2018) Scientific
#' Data paper: https://doi.org/10.1038/sdata.2019.17.
#' @param nhm_identifier_col if `network` is "nhm" identify which column in
#' `confinement_data` contains the unique identifier to use for aggregation (e.g.
#' "PRMS_segid" or "seg_id_nat"). Defaults to "seg_id_nat".
#' 
#' @returns 
#' Returns a data frame with one row per reach/segment and columns representing
#' the valley bottom length: river length ratio, the valley bottom area: river
#' area ratio, and the confinement category that was assigned ("Confinement_calc").
#' 
aggregate_mcmanamay_confinement <- function(confinement_data, 
                                            nhd_nhm_xwalk, 
                                            force_min_width_m = 0,
                                            preferred_width_df = NULL,
                                            network = "nhdv2", 
                                            nhm_identifier_col = "seg_id_nat"){
  
  # Check that the value for `network` matches one of two options we expect
  if(!network %in% c("nhdv2","nhm")){
    stop(paste0("The network argument accepts 'nhdv2' or 'nhm'. Please check ",
                "that the requested network matches one of these two options."))
  }
  
  # 1) Subset full confinement dataset to the NHDPlusv2 COMID's of interest.
  confinement_subset <- confinement_data %>%
    filter(COMID %in% nhd_nhm_xwalk$COMID) %>%
    mutate(COMID = as.character(COMID))

  # 2) Format the confinement dataset. Note that McManamay and DeRolph develop
  # a categorization scheme (described in https://doi.org/10.1038/sdata.2019.17) 
  # to assign confinement categories: "unconfined," "moderately confined," or 
  # "confined." Even though they only present confinement classes, we can also
  # calculate a numeric value for confinement based on the information given.
  confinement_subset_proc <- confinement_subset %>%
    rename(reach_length_km = RL,
           valley_bottom_length = VBL,
           reach_area = RWA,
           valley_bottom_area = VBA,
           vbl_rl_ratio = VBL_RL_R,
           vba_ra_ratio = VBA_RWA_R) %>%
    # Back-calculate river width and floodplain width from the values given.
    mutate(river_width_m_mcmanamay = reach_area/(reach_length_km*1000),
           floodplain_width_m_mcmanamay = if_else(valley_bottom_length == 0, 
                                        NA_real_,
                                        valley_bottom_area/(valley_bottom_length*1000))) %>%
    # Replace all values of width below the user-specified value in `force_min_width_m`.
    # If `force_min_width_m` equals zero, this step is functionally omitted. 
    mutate(river_width_m_mcmanamay = if_else(river_width_m_mcmanamay < force_min_width_m, 
                                             force_min_width_m, 
                                             river_width_m_mcmanamay)) 
  
  # 3) Calculate channel confinement. If the user specifies an alternative data
  # source for river width, join those values to the subset McManamay confinement
  # dataset and use the preferred widths to estimate channel confinement. 
  if(!is.null(preferred_width_df)){
    confinement_nhd <- confinement_subset_proc %>%
      left_join(y = preferred_width_df, by = "COMID") %>%
      # Note that the confinement calculation below is equal to vba_ra_ratio/vbl_rl_ratio. 
      # If the denominator is equal to zero, just assign NA for channel confinement.
      mutate(confinement_calc_mcmanamay = if_else(width_m == 0 | floodplain_width_m_mcmanamay == 0,
                                                  NA_real_,
                                                  floodplain_width_m_mcmanamay/width_m)) %>%
      rename(river_width_m = width_m)
  # Otherwise, use width values derived from McManamay variables:
  } else {
    confinement_nhd <- confinement_subset_proc %>%
      mutate(confinement_calc_mcmanamay = if_else(river_width_m_mcmanamay == 0 | floodplain_width_m_mcmanamay == 0,
                                                  NA_real_,
                                                  floodplain_width_m_mcmanamay/river_width_m_mcmanamay)) %>%
      rename(river_width_m = river_width_m_mcmanamay)
  }

  # 4) If requested network is "nhdv2", format columns and return confinement estimates.
  if(network == "nhdv2"){
    confinement_nhd_out <- confinement_nhd %>%
      rename(floodplain_width_m = floodplain_width_m_mcmanamay) %>%
      select(COMID, reach_length_km, river_width_m, floodplain_width_m, confinement_calc_mcmanamay)
    return(confinement_nhd_out)
  }
  # Otherwise, if requested network is "nhm", aggregate NHDPlusv2 values to NHM segments.
  if(network == "nhm"){
    # Join subsetted confinement dataset to NHM segment identifiers and format
    # column names in preparation for aggregation step.
    confinement_w_nhm_segs <- confinement_nhd %>%
      rename(reach_length_km_comid = reach_length_km,
             confinement_calc_comid = confinement_calc_mcmanamay,
             river_width_comid = river_width_m,
             floodplain_width_comid = floodplain_width_m_mcmanamay) %>%
      left_join(nhd_nhm_xwalk, by = "COMID")
    
    # Group data by NHM segment and calculate a length-weighted average of the
    # confinement values among individual COMIDs that make up the NHM segment.
    # Include columns used to assess coverage and flag McManamay confinement estimates.
    confinement_nhm <- confinement_w_nhm_segs %>%
      group_by(.data[[nhm_identifier_col]]) %>%
      summarize(
        reach_length_km = sum(reach_length_km_comid),
        lengthkm_mcmanamay_is_na = sum(reach_length_km_comid[is.na(confinement_calc_comid)]),
        prop_reach_w_mcmanamay = 1-(lengthkm_mcmanamay_is_na/reach_length_km),
        confinement_calc_mcmanamay = if_else(prop_reach_w_mcmanamay > 0,
                                             weighted.mean(x = confinement_calc_comid,
                                                           w = reach_length_km_comid,
                                                           na.rm = TRUE),
                                             NA_real_),
        .groups = "drop") %>%
      mutate(
        flag_mcmanamay = if_else(prop_reach_w_mcmanamay < 0.7, 
                             paste0("Note that <70% of the NHM segment is ",
                                    "covered by a COMID with McManamay confinement data."),
                             NA_character_)
      )

    return(confinement_nhm)
  }
  
}



#' @title Estimate channel confinement from FACET geomorphic data
#' 
#' @description
#' Function to aggregate geomorphic data derived from 3-m LiDAR from the
#' DRB FACET dataset to estimate channel confinement for individual 
#' NHDPlusv2 or NHM segments.
#' 
#' @details 
#' See FACET metadata for further details about the geomorphic dataset
#' obtained from the FACET model and the geomorphic metric columns used
#' for `facet_width_col` and `facet_floodplain_width_col` here.
#' https://doi.org/10.5066/P9RQJPT1.
#' 
#' @param facet_network sf linestring object containing the FACET stream network.
#' Must contain columns "UniqueID", Magnitude", and "USContArea" in addition to 
#' the columns defined in `facet_width_col` and `facet_floodplain_width_col`.
#' @param facet_width_col character string indicating which column from the FACET
#' dataset should be used to represent channel width. Defaults to "CW955mean_1D", 
#' channel width, mean, for values <95th and >5th percentile (within the reach). 
#' @param facet_floodplain_width_col character string indicating which column from
#' the FACET dataset should be used to represent floodplain width. Defaults to
#' "FWmean_1D_FP", total floodplain width (fpwid_1d), mean. 
#' @param nhd_catchment_polygons sf polygon object containing the NHDPlusv2
#' catchments. Must contain column "COMID".
#' @param nhd_nhm_xwalk data frame that specifies how NHDPlusv2 COMIDs map
#' onto NHM segment identifiers. Must contain columns "COMID", "PRMS_segid",
#' and "seg_id_nat".
#' @param network character string indicating the requested resolution of the
#' channel confinement values. Options include "nhdv2" or "nhm".
#' @param nhm_identifier_col if `network` is "nhm" identify which column in
#' `confinement_data` contains the unique identifier to use for aggregation (e.g.
#' "PRMS_segid" or "seg_id_nat"). Defaults to "seg_id_nat".
#' @param show_warnings logical; should any warnings that arise during the 
#' spatial join be printed to the console? Defaults to FALSE.
#' 
#' @returns 
#' Returns a data frame containing estimates of channel confinement, defined
#' as floodplain width/channel width (unitless). 
#' 
calculate_facet_confinement <- function(facet_network, 
                                        facet_width_col = "CW955mean_1D", 
                                        facet_floodplain_width_col = "FWmean_1D_FP", 
                                        nhd_catchment_polygons, 
                                        nhd_nhm_xwalk,
                                        network = "nhdv2",
                                        nhm_identifier_col = "seg_id_nat",
                                        show_warnings = FALSE){
  
  # Check that the value for `network` matches one of two options we expect.
  if(!network %in% c("nhdv2","nhm")){
    stop(paste0("The network argument accepts 'nhdv2' or 'nhm'. Please check ",
                "that the requested network matches one of these two options."))
  }
  
  # Spatially join the FACET stream network with the NHDPlusv2 catchments.
  # Then, for each NHDPlusv2 catchment, subset the FACET segment with the 
  # largest Shreve magnitude (if multiple segments with the same magnitude, 
  # break a tie using the upstream area). Capture all warning messages (e.g.
  # "attribute variables assumed to be spatially constant throughout all 
  # geometries" during the st_intersection step). If `show_warnings` is TRUE, 
  # print any warning messages to the console. Otherwise, hide them.
  facet_nhd <- withCallingHandlers({
    sf::st_intersection(x = facet_network, y = nhd_catchment_polygons) %>%
      group_by(COMID) %>%
      arrange(desc(Magnitude), desc(USContArea)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(COMID = as.character(COMID))
  }, warning = function(w) {
    if(!show_warnings) invokeRestart("muffleWarning")
  })


  # Retain selected columns in the joined FACET dataset and subset
  # the data to only include the requested COMIDs.
  cols_to_keep <- c("COMID", "UniqueID", "HUC4", "Magnitude", "USContArea", 
                    facet_width_col, facet_floodplain_width_col)
  
  facet_nhd_out <- nhd_nhm_xwalk %>%
    left_join(y = facet_nhd, by = "COMID") %>%
    select(any_of(cols_to_keep)) %>%
    sf::st_drop_geometry() %>%
    rename(channel_width := {{facet_width_col}}) %>%
    rename(floodplain_width := {{facet_floodplain_width_col}}) %>%
    mutate(confinement_calc_facet = floodplain_width/channel_width)
  
  # If network = "nhdv2", return estimated confinement values.
  if(network == "nhdv2"){
    return(facet_nhd_out)
  }
  
  
  if(network == "nhm"){
    # If network = "nhm", download requested COMIDs and subset reach
    # length column. Then join NHD-scale metrics to NHM segment IDs.
    nhd_lengths <- nhdplusTools::get_nhdplus(comid = unique(nhd_nhm_xwalk$COMID),
                                             realization = "flowline") %>%
      mutate(COMID = as.character(comid)) %>%
      sf::st_drop_geometry() %>%
      select(COMID, lengthkm) %>%
      rename(lengthkm_comid = lengthkm)
    
    # Bind COMID, NHDPlusv2 reach lengths, and NHDPlusv2-scale FACET
    # metrics together.
    facet_nhm <- nhd_nhm_xwalk %>%
      left_join(y = nhd_lengths, by = "COMID") %>%
      left_join(y = facet_nhd_out, by = "COMID") %>%
      rename(confinement_calc_comid = confinement_calc_facet)
    
    # For each NHM segment, calculate a length-weighted average of the NHDPlusv2-scale
    # channel confinement values. In addition, add a flag for any NHM segments where 
    # less than 70% of the segment is covered by a COMID with FACET values.
    facet_nhm_out <- facet_nhm %>%
      group_by(.data[[nhm_identifier_col]]) %>%
      summarize(
        lengthkm = sum(lengthkm_comid),
        lengthkm_facet_is_na = sum(lengthkm_comid[is.na(confinement_calc_comid)]),
        prop_reach_w_facet = 1-(lengthkm_facet_is_na/lengthkm),
        confinement_calc_facet = if_else(prop_reach_w_facet > 0,
                                         weighted.mean(x = confinement_calc_comid, 
                                                       w = lengthkm_comid, 
                                                       na.rm = TRUE),
                                         NA_real_)) %>%
      mutate(
        flag_facet = if_else(prop_reach_w_facet < 0.7, 
                             paste0("Note that <70% of the NHM segment is ",
                                    "covered by a COMID with FACET data."),
                             NA_character_)
      )
    
    return(facet_nhm_out)
  }
  
}



#' @title Get centroid of stream reaches
#' 
#' @description 
#' Function to find the centroid of each linestring object representing
#' one stream reach within the network.
#' 
#' @param network sf linestring object containing the stream network. Must
#' contain column "UniqueID".
#' @param show_warnings logical; should any warnings that arise during the 
#' spatial join be printed to the console? Defaults to FALSE.
#' 
#' @returns 
#' Returns sf point object containing one centroid point per linestring in `network`.
#' 
get_reach_centroids <- function(network, show_warnings = FALSE){
  
  message("Finding the centroid for each reach within the network. This may take awhile...")
  
  network_pts_at_centroids <- network %>%
    split(., .$UniqueID) %>%
    lapply(., function(x){
      # 1) cast the reach to points:
      reach_pts <- withCallingHandlers({
        sf::st_cast(x, "POINT")
      }, warning = function(w){
        if(!show_warnings) invokeRestart("muffleWarning")
      })
      
      # 2) Find the centroid of each reach:
      reach_centroid <- withCallingHandlers({
        sf::st_centroid(x)
      }, warning = function(w){
        if(!show_warnings) invokeRestart("muffleWarning")
      })
      
      # 3) Snap reach centroid to the reach:
      pt_at_centroid <- reach_pts[which.min(sf::st_distance(reach_centroid, reach_pts)),]
      return(pt_at_centroid)
    }) %>%
    bind_rows()

  return(network_pts_at_centroids)
}


#' @title Fill in NA values from upstream/downstream neighboring reaches
#' 
#' @description 
#' Function to fill in a reach's attribute value with a value from its
#' neighboring reaches (either upstream neighbor or downstream neighbor).
#' 
#' @details 
#' This function was inspired by code initially developed as part of the
#' drb-inland-salinity-ml project.
#' https://github.com/USGS-R/drb-inland-salinity-ml/blob/main/2_process/src/process_nhdv2_attr.R#L483-L559
#' 
#' @param attr_df data frame containing the attribute data. 
#' @param nhm_identifier_col character string indicating the name of the column
#' that contains the unique segment identifier (e.g. "PRMS_segid" or "seg_id_nat").
#' Defaults to "seg_id_nat".
#' @param attr_name character string indicating the attribute column name.
#' @param reach_distances data frame representing the upstream-downstream 
#' connections among segments within the river network, with rows representing
#' the "from" segments and columns representing the "to" segments. Positive 
#' values indicate that the "to" segment is downstream of the "from" segment; 
#' negative values indicate that the "to" segment is upstream of the "from" 
#' segment.
#' @param neighbors character string indicating whether NA values should be 
#' imputed using "upstream" neighbors, "downstream" neighbors, or "nearest" 
#' neighbors. Defaults to "upstream."
#' 
#' @returns 
#' Returns `attr_df` with NA values filled for the `attr_name` column.
#' 
refine_from_neighbors <- function(attr_df, 
                                  nhm_identifier_col = "seg_id_nat", 
                                  attr_name, 
                                  reach_distances,
                                  neighbors = "upstream"){
  
  # Check that the value for `neighbors` matches one of three options we expect.
  if(!neighbors %in% c("upstream","downstream","nearest")){
    stop(paste0("The neighbors argument accepts 'upstream', 'downstream', or ",
                "'nearest'. Please check that the requested method matches one ",
                "of these three options."))
  }
  
  # 1) Find reaches with NA values for `attr_name`.
  ind_reach <- attr_df %>%
    filter(is.na(.data[[attr_name]])) %>%
    pull(.data[[nhm_identifier_col]])
  
  # 2) Define a function to find neighboring, non-NA segments
  find_neighbors <- function(reach_select, attr_df, nhm_identifier_col, 
                             attr_name, reach_distances, neighbors){
    
    # subset attribute data frame and format columns
    attr_df_select <- attr_df %>%
      select(all_of(c(nhm_identifier_col, attr_name))) %>%
      rename(attr_value := !!attr_name)
    
    # calculate global median value of attr_name before NA values are filled
    attr_value_median <- median(attr_df_select$attr_value, na.rm = TRUE)
    
    # format reach distance data frame and append subsetted attribute data frame
    cols_keep <- c("from", reach_select)
    seg_matches <- reach_distances %>%
      select(all_of(cols_keep)) %>%
      mutate(!!nhm_identifier_col := as.character(from)) %>%
      rename(target_reach := !!reach_select) %>%
      left_join(attr_df_select, by = nhm_identifier_col) %>%
      # keep segments with an upstream-downstream connection to `reach_select`
      filter(target_reach != "Inf")
    
    # subset attribute df to nearest non-NA segments, considering either upstream
    # or downstream segments depending on the option defined in `neighbors`.
    if(neighbors == "upstream"){
      seg_matches_proc <- seg_matches %>%
        # identify nearest [upstream] neighboring segments
        filter(target_reach > 0) %>%
        arrange(target_reach) %>%
        # only consider neighboring segments that have a non-NA value
        filter(!is.na(attr_value)) %>%
        # select nearest [upstream] neighbor
        slice(1) %>%
        mutate(flag_gaps = sprintf("%s was filled from neighbors: %s (%s km away).", 
                                   attr_name, .data[[nhm_identifier_col]], round(abs(target_reach)/1000,1)),
               seg_id = reach_select) %>%
        select(seg_id, attr_value, flag_gaps) %>%
        rename(!!nhm_identifier_col := seg_id)
    }
    if(neighbors == "downstream"){
      seg_matches_proc <- seg_matches %>%
        # identify nearest [downstream] neighboring segments
        filter(target_reach < 0) %>%
        arrange(desc(target_reach)) %>%
        # only consider neighboring segments that have a non-NA value
        filter(!is.na(attr_value)) %>%
        # select nearest [downstream] neighbor
        slice(1) %>%
        mutate(flag_gaps = sprintf("%s was filled from neighbors: %s (%s km away).", 
                                   attr_name, .data[[nhm_identifier_col]], round(abs(target_reach)/1000,1)),
               seg_id = reach_select) %>%
        select(seg_id, attr_value, flag_gaps) %>%
        rename(!!nhm_identifier_col := seg_id)
    }
    if(neighbors == "nearest"){
      seg_matches_proc <- seg_matches %>%
        # identify nearest neighboring segments
        arrange(abs(target_reach)) %>%
        # only consider neighboring segments that have a non-NA value
        filter(!is.na(attr_value)) %>%
        # select nearest neighbor
        slice(1) %>%
        mutate(flag_gaps = sprintf("%s was filled from neighbors: %s (%s km away).", 
                                   attr_name, .data[[nhm_identifier_col]], round(abs(target_reach)/1000,1)),
               seg_id = reach_select) %>%
        select(seg_id, attr_value, flag_gaps) %>%
        rename(!!nhm_identifier_col := seg_id)
    }
    
    # 3) If attribute value is still NA after trying to impute using neighboring
    # reaches, fill value using the global median.
    if(length(seg_matches_proc$seg_id_nat) < 1){
      seg_matches_proc <- seg_matches %>%
        arrange(abs(target_reach)) %>%
        # keep reach_select only (distance = 0)
        slice(1) %>%
        mutate(flag_gaps = sprintf("%s was filled using median value from non-NA segments.", 
                                   attr_name),
               seg_id = reach_select,
               attr_value = attr_value_median) %>%
        select(seg_id, attr_value, flag_gaps) %>%
        rename(!!nhm_identifier_col := seg_id)
    }
    
    return(seg_matches_proc)
  }
  
  # 3) For each reach with NA values, find the nearest non-NA neighbors
  ind_reach_replace <- ind_reach %>%
    lapply(., find_neighbors, attr_df, nhm_identifier_col, attr_name, reach_distances, neighbors) %>%
    bind_rows()
  
  # 4) Fill NA values with non-NA neighbors
  attr_df_filled <- attr_df %>% 
    rename(attr_value := !!attr_name) %>%
    # join original attr_df with df containing replacement from neighbors
    left_join(y = ind_reach_replace, by = nhm_identifier_col) %>%
    # use coalesce function to find first value that is not NA, from 
    # original attr_df or from `ind_reach_replace`.
    mutate(!!attr_name := coalesce(attr_value.x, attr_value.y)) %>%
    select(c(names(attr_df)), flag_gaps)
    
  return(attr_df_filled)
}
  
