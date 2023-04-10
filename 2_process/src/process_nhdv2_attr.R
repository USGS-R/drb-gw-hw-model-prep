#' @title Process NHDPlusv2 attributes for cumulative upstream watershed
#' 
#' @description 
#' Function to read in downloaded NHDv2 attribute data and join with river segment ID's.
#' 
#' @details This function was pulled and modified from the inland salinity ml project:
#' https://github.com/USGS-R/drb-inland-salinity-ml/blob/main/2_process/src/process_nhdv2_attr.R
#'
#' @param file_path file path of downloaded NHDv2 attribute data table, including
#' file extension
#' @param segs_w_comids data frame containing the PRMS segment ids and the comids
#' of interest. Must contain variables "PRMS_segid" and "COMID".
#' @param cols character string indicating which columns to retain from downloaded
#' attribute data; cols can take values "ACC" or "TOT".
#'
#' @return 
#' A data frame containing PRMS_id and columns representing the NHDv2 attribute
#' data referenced to the cumulative upstream watershed.
#' 
process_cumulative_nhdv2_attr <- function(file_path, segs_w_comids, cols){

  message(file_path)
  # Read in downloaded data 
  # only specify col_type for COMID since cols will differ for each downloaded data file
  dat <- read_csv(file_path, col_types = cols(COMID = "c"), show_col_types = FALSE)

  # LAUREN: Commenting out the two lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # For PPT data we want to return the long-term (1971-2000) monthly averages 
  # instead of the monthly values for each year
  #if(grepl("PPT_TOT",file_path)|grepl("PPT_ACC",file_path)){
  #  message("Calculating long-term monthly average precipitation from annual data")
  #  dat <- calc_monthly_avg_ppt(dat)
  #}
  
  # LAUREN: Commenting out the two lines below that are used in inland salinity
  # but not for our groundwater data prep.
  # For NADP data we want to return the long-term (1984-2014) average 
  # instead of annual values for each constituent
  #if(grepl("NADP",file_path)){
  #  message("Calculating long-term average NADP from annual data")
  #  dat <- calc_avg_NADP(dat)
  #}
    
  # Process downloaded data
  dat_proc <- dat %>%
    # retain desired columns ('ACC' or 'TOT')
    select(c(COMID, starts_with(cols))) %>%
    # join data to {segs_w_comids} data frame by COMID
    right_join(., segs_w_comids,by = "COMID") %>%
    relocate(names(segs_w_comids), .before = "COMID") %>%
    select(-COMID)
  
  # Flag columns with undesired flag values (e.g. -9999)
  flag_cols <- dat_proc %>%
    select(where(function(x) -9999 %in% x)) %>% 
    names()
  
  # For columns with undesired flag values, replace -9999 with NA, else use existing value
  dat_proc_out <- dat_proc %>%
    mutate(across(all_of(flag_cols), 
                  ~case_when(. == -9999 ~ NA_real_, 
                             TRUE ~ as.numeric(.))))

  return(dat_proc_out)
  
}



#' @title Process catchment-scale NHDPlusv2 attributes to NHM catchments
#' 
#' @description 
#' Function to read in downloaded NHDPlusv2 attribute data and aggregate to NHM 
#' river segment ID's based on the CAT aggregation operation defined in the 
#' attributes table ("1_fetch/in/nhdv2_attributes_from_sciencebase.csv)
#' 
#' @details This function was pulled and modified from the inland salinity ml project:
#' https://github.com/USGS-R/drb-inland-salinity-ml/blob/main/2_process/src/process_nhdv2_attr.R
#'
#' @param file_path file path of downloaded NHDv2 attribute files, including 
#' file extension
#' @param vars_table data frame containing list of attributes from ScienceBase 
#' that will be processed. Must contain columns "SB_dataset_name" and
#' "CAT_aggregation_operation".
#' @param segs_w_comids data frame containing the PRMS segment ids and the COMIDs 
#' of interest. Must contain column "COMID" as well as a column that represents a
#' unique segment identifier (defined in `nhm_identifier_col`).
#' @param nhm_identifier_col character string that represents the unique segment 
#' identifier. This identifier is used to inform how we aggregate catchment attributes.
#' Examples include "PRMS_segid" or "seg_id_nat".
#' @param nhd_lines sf object containing NHDPlusV2 flowlines for area of interest.
#' nhd_lines must contain columns "COMID", "AREASQKM", and "LENGTHKM".
#'
#' @return 
#' Returns a list that includes a data table containing the PRMS_id and 
#' columns representing the NHDPlusv2 attribute data scaled to the local NHM 
#' catchment, and a data table containing NA diagnostic information for each 
#' variable and PRMS segment.
#'
process_catchment_nhdv2_attr <- function(file_path, vars_table, segs_w_comids, nhm_identifier_col, nhd_lines){
  

  # Before starting any processing steps, check that this function will work for
  # the requested data since we've commented out some of the lines below that we 
  # don't anticipate using for groundwater. Then inform the user.
  check_files <- grepl("PPT_CAT|NADP", file_path)
  check_sb_ids <- grepl("58c301f2e4b0f37a93ed915a|
                        5910de31e4b0e541a03ac983|
                        5734acafe4b0dae0d5de622d|
                        57e2ac2fe4b0908250045981", vars_table$sb_id)
  check_all <- c(check_files, check_sb_ids)
  if(any(check_all)){
    stop(sprintf(paste0("The function process_catchment_nhdv2_attr() is not ",
    "currenty formatted to process file %s. The function may work if certain ",
    "lines are uncommented."), file_path))
  } else {
    message(paste0("Processing SB attribute file: ", file_path))
  }
  
  
  # 1. Parse dataset name from file_path
  data_name <- str_split(basename(file_path),".[[:alnum:]]+$")[[1]][1]
  
  # 2. Format inputs {nhd_lines} and {segs_w_comids} 
  nhd_reaches <- nhd_lines %>%
    # reformat variable names to uppercase
    rename_with(.,toupper,id:enabled) %>%
    sf::st_drop_geometry() %>%
    select(COMID, AREASQKM, LENGTHKM) %>%
    mutate(COMID = as.character(COMID))
  
  # This line makes sure that the column containing the COMIDs is capitalized
  segs_w_comids <- segs_w_comids %>%
    rename(COMID = grep("comid", names(segs_w_comids), ignore.case = TRUE, value = TRUE))
  
  # 3. Read in downloaded data file
  # only specify col_type for COMID since cols will differ for each downloaded data file
  dat <- read_csv(file_path, col_types = cols(COMID = "c"), show_col_types = FALSE)
  
  # LAUREN: Commenting out the PPT lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # For PPT data we want to return the long-term (1971-2000) monthly averages 
  # instead of the monthly values for each year
  #if(grepl("PPT_CAT",file_path)){
  #  message("Calculating long-term monthly average precipitation from annual data")
  #  dat <- calc_monthly_avg_ppt(dat)
  #}
  
  # LAUREN: Commenting out the NADP lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # For NADP data we want to return the long-term (1985-2014) average 
  # instead of annual values
  #if(grepl("NADP",file_path)){
  #  message("Calculating long-term average NADP from annual data")
  #  dat <- calc_avg_NADP(dat)
  #}
  
  # 4. Identify columns of interest
  # Subset VarsOfInterest table to retain the desired dataset {data_name} 
  vars_item <- vars_table %>%
    filter(SB_dataset_name == data_name) 
  
  col_names <- tibble(col_name_orig = vars_item$attribute_name,
                      cat_agg_op = vars_item$CAT_aggregation_operation) %>%
    filter(!is.na(col_name_orig)) %>%
    mutate(col_name = ifelse(col_name_orig == "sinuosity",
                             "sinuosity",
                             paste0("CAT_",col_name_orig))) %>%
    select(col_name, cat_agg_op)
  
  # Reformat col_names for certain datasets with year suffixes on column names
  # LAUREN: Commenting out the NID lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # Function `format_col_names_years` sourced from 1_fetch/fetch_nhdv2_attributes_from_sb.R
  # National Inventory of Dams data:
  #if(unique(vars_item$sb_id) == "58c301f2e4b0f37a93ed915a"){
  #  years <- unique(str_extract(names(dat),"\\d{2,}"))
  #  years <- years[!is.na(years)]
  #  cols <- format_col_names_years(col_names$col_name,years,yr_pattern = "YYYY")
  #  cols <- cols[cols != "COMID"]
  #  col_names <- tibble(col_name = cols,
  #                      cat_agg_op = rep(vars_item$CAT_aggregation_operation,length(years))) 
  #}
  
  # LAUREN: Commenting out the HDENS lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # HDENS data:
  #if(unique(vars_item$sb_id) == "5910de31e4b0e541a03ac983"){
  #  years <- unique(str_extract(names(dat),"\\d{2,}"))
  #  years <- years[!is.na(years)]
  #  cols <- format_col_names_years(col_names$col_name,years,yr_pattern = "XX")
  #  cols <- cols[cols != "COMID"]
  #  col_names <- tibble(col_name = cols,
  #                      cat_agg_op = rep(vars_item$CAT_aggregation_operation,length(years)))
  #}
  
  # LAUREN: Commenting out the PPT lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # monthly average precipitation data:
  #if(unique(vars_item$sb_id) %in% c("5734acafe4b0dae0d5de622d")){
  #  col_names$col_name <- names(dat)[names(dat) != "COMID"]
  #}
  
  # LAUREN: Commenting out the NADP lines below that are used in inland salinity but
  # not for our groundwater data prep.
  # NADP data:
  #if(unique(vars_item$sb_id) == "57e2ac2fe4b0908250045981"){
  #  col_names$col_name <- str_replace(col_names$col_name, pattern = "_YYYY", 
  #                                    replacement = "")
  #}
  
  # Separate columns based on the aggregation operation we need to scale the 
  # NHD-catchment-values to PRMS-scale values.
  cols_sum <- col_names$col_name[col_names$cat_agg_op == "sum"]
  cols_area_wtd_mean <- col_names$col_name[col_names$cat_agg_op == "area_weighted_mean"]
  cols_length_wtd_mean <- col_names$col_name[col_names$cat_agg_op == "length_weighted_mean"]
  cols_min <- col_names$col_name[col_names$cat_agg_op == "min"]
  cols_max <- col_names$col_name[col_names$cat_agg_op == "max"]
  
  # 5. Munge downloaded data
  dat_proc <- dat %>%
    # retain CAT columns from NHDv2 attributes dataset
    select(c(COMID, starts_with("CAT"))) %>%
    # join data to {segs_w_comids} data frame by COMID
    right_join(.,segs_w_comids,by = c("COMID")) %>%
    # join data to {nhd_reaches} by COMID since we need the AREASQKM and LENGTHKM
    # attributes not included in the downloaded NHDv2 attribute datasets.
    left_join(.,nhd_reaches,by = "COMID") %>%
    # approximate NHDv2 catchment area for all COMID's where AREASQKM equals zero. 
    # If catchment area is greater than zero, just use AREASQKM.
    mutate(AREASQKM_approx = case_when(AREASQKM == 0 ~ LENGTHKM^2, TRUE ~ AREASQKM)) %>%
    # format columns
    relocate(names(segs_w_comids),.before = everything()) 
  
  # 5b. Handle missing values and flagged values
  # Flag columns with undesired flag values (e.g. -9999)
  flag_cols <- dat_proc %>%
    select(where(function(x) -9999 %in% x | any(is.na(x)))) %>% 
    names()
  
  # Before replacing flagged values, tally the number of -9999's as well as the 
  # proportion of total NHD area where the value is -9999 to use for diagnostics
  flag_tally <- dat_proc %>%
    group_by(.data[[nhm_identifier_col]]) %>%
    summarize(
      AREASQKM_PRMS = sum(AREASQKM_approx), 
      num_NHDv2cats = length(unique(COMID)),
      across(all_of(flag_cols), 
             ~length(which(. == -9999 | is.na(.))), 
             .names = "{col}_num_NA"),
      across(all_of(flag_cols), 
             ~round(sum(AREASQKM_approx[which(. == -9999 | is.na(.))]/AREASQKM_PRMS), 4), 
             .names = "{col}_propAREA_NA")
    )
  
  # 5c. For columns with undesired flag values, replace -9999 with NA, else use existing value
  
  # For STATSGO variables related to HYDGRP, TEXT, and LAYER, the metadata indicate 
  # that -9999 denotes NODATA usually water. For these soils variables only, 
  # change -9999 values to 0.
  statsgo_sbid <- c("5728d93be4b0b13d3918a99f","5728decfe4b0b13d3918a9aa","5728dd46e4b0b13d3918a9a7")
  if(unique(vars_item$sb_id) %in% statsgo_sbid){
    dat_proc_out <- dat_proc %>%
      mutate(across(all_of(flag_cols), ~case_when(. == -9999 ~ 0, TRUE ~ as.numeric(.))))
  } else {
    dat_proc_out <- dat_proc %>%
      mutate(across(all_of(flag_cols), ~case_when(. == -9999 ~ NA_real_, TRUE ~ as.numeric(.))))
  }
  
  # 6. Scale NHDv2 attributes to PRMS catchments
  dat_proc_aggregated <- dat_proc_out %>%
    # summarize the data for each unique segment identifier
    group_by(.data[[nhm_identifier_col]]) %>%
    # apply desired aggregation operations to appropriate columns
    summarize(
      AREASQKM_PRMS = sum(AREASQKM_approx), 
      LENGTHKM_PRMS = sum(LENGTHKM),
      across(any_of(cols_area_wtd_mean), weighted.mean, w = AREASQKM_approx, na.rm = TRUE, .names = "{col}_area_wtd"),
      across(any_of(cols_length_wtd_mean), weighted.mean, w = LENGTHKM, na.rm = TRUE, .names = "{col}_length_wtd"),
      across(any_of(cols_sum), sum, na.rm = T, .names = "{col}_sum"),
      across(any_of(cols_min), min, na.rm = T, .names = "{col}_min"),
      across(any_of(cols_max), max, na.rm = T, .names = "{col}_max")) %>%
    # For segments where all values of a column were NA (thus generating NAN's), replace NAN with NA
    mutate(across(where(is.numeric), ~if_else(is.nan(.), NA_real_, .))) %>%
    ungroup()
  
  # 7. Return a list containing the aggregated data table and a table with NA diagnostics for each 
  # attribute and segment ID.
  dat_proc_list <- list(data = dat_proc_aggregated, NA_diagnostics = flag_tally)
  
  return(dat_proc_list)
  
}



#' @title Combine re-aggregated NHDPlusv2 attributes to a single data frame
#' 
#' @description 
#' Function to combine two lists containing cumulative upstream and catchment-scale
#' NHDv2 attributes that have been re-processed to the NHM scale. 
#' 
#' @details This function was pulled and modified from the inland salinity ml project:
#' https://github.com/USGS-R/drb-inland-salinity-ml/blob/main/2_process/src/process_nhdv2_attr.R
#' 
#' @param attr_data_upstream list object containing NHDv2 attributes referenced to 
#' the cumulative upstream watershed
#' @param attr_data_catchment list object containing NHDv2 attributes scaled to 
#' the local contributing catchment; attr_data_catchment should be a nested list
#' such that the first element represents a unique attribute dataset and the second
#' element represents data frames containing the aggregated datasets and the NA 
#' diagnostics for the attribute dataset. 
#' @param nhm_identifier_col character string that represents the unique segment 
#' identifierused in both the upstream and catchment data sets. Examples include 
#' "PRMS_segid" or "seg_id_nat".
#' 
#' @value 
#' returns a data frame with one row per NHM segment and one column for 
#' each unique NHDv2 attribute variable.
#'
create_nhdv2_attr_table <- function(attr_data_upstream, attr_data_catchment, nhm_identifier_col){

  # For the attribute variables scaled to the PRMS catchment-scale, select 
  # the list elements containing the data and omit the list elements 
  # containing the NA diagnostics information. 
  attr_data_catchment_dat <- lapply(attr_data_catchment, "[[", 1)
  
  # loop through both lists simultaneously and join data frames by PRMS_segid
  attr_data_df <- purrr::map2(attr_data_catchment_dat, attr_data_upstream, 
                              full_join, by = nhm_identifier_col) %>%
    # bind all columns containing NHDv2 attributes into a single data frame
    Reduce(full_join,.) %>%
    # hide messages that data frames are being joined by column 'PRMS_segid'
    suppressMessages()
  
  return(attr_data_df)
  
}


