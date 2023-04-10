#' @title Prepare NHD-scale static input drivers
#' 
#' @description 
#' Function to combine river-dl static input drivers for NHDPlusv2 reaches, 
#' including mean width, reach slope, and reach elevation.
#' 
#' @param nhd_flowlines sf object containing NHDPlusv2 flowline reaches. Must
#' contain columns "comid", "minelevsmo", "maxelevsmo", "slope"
#' @param prms_inputs data frame containing input drivers for each NHM segment.
#' Must include columns "seg_id_nat", "seg_elev", "seg_slope", and "seg_width"
#' @param nhd_nhm_xwalk data frame that specifies how NHDPlusv2 COMIDs map
#' onto NHM segment identifiers. Must contain columns "COMID", "PRMS_segid",
#' and "seg_id_nat".
#'
#' @returns 
#' Returns a data frame with one row per COMID in `nhd_flowlines`. Columns
#' indicate the NHM segment identifiers, the mean width for the NHD reach 
#' ("est_width_m"), the slope of the NHD reach ("slope"), the length-weighted
#' NHD slope for the COMIDs that make up each NHM segment ("slope_len_wtd_mean"), 
#' the length of the NHD reach ("lengthkm"), the min and max NHD reach elevation
#' ("min_elev_m" and "max_elev_m"), the elevation, slope, and mean width of the 
#' corresponding NHM segment ("seg_elev", "seg_slope", and "seg_width"), the
#' maximum NHD reach width among the COMIDs that make up each NHM segment
#' ("seg_width_max"), and the minimum NHD slope among the COMIDs that make up
#' each NHM segment ("seg_elev_min").
#' 
prepare_nhd_static_inputs <- function(nhd_flowlines, prms_inputs, nhd_nhm_xwalk){
  
  # Subset NHDPlusv2 flowlines to return the desired attributes, 
  # including elevation and slope
  nhd_attributes <- nhd_flowlines %>%
    sf::st_drop_geometry() %>%
    # transform elevation from cm to meters
    mutate(COMID = as.character(comid),
           min_elev_m = minelevsmo/100, 
           max_elev_m = maxelevsmo/100,
           slope = if_else(slope == -9998, NA_real_, slope)) 
  
  # Add NHM segment identifier to NHDv2 attributes table
  nhd_attributes_w_nhm <- nhd_attributes %>%
    left_join(y = nhd_nhm_xwalk, by = "COMID") %>%
    rename(subsegid = PRMS_segid)
  
  # Format NHDPlusv2 input data.
  # calculate length-weighted average slope for NHDv2 reaches associated
  # with each NHM reach. For simplicity, weight by the reach length rather
  # than another value-added attribute, slopelenkm, which represents the
  # length over which the NHDv2 attribute slope was computed.
  nhd_static_inputs <- nhd_attributes_w_nhm %>%
    group_by(subsegid) %>%
    mutate(slope_len_wtd_mean = weighted.mean(x = slope, w = lengthkm, na.rm = TRUE),
           seg_width_max = max(est_width_m, na.rm = TRUE), 
           seg_elev_min = min(min_elev_m, na.rm = TRUE)) %>%
    ungroup() %>%
    # join select attributes from PRMS-SNTemp
    left_join(y = prms_inputs, by = "seg_id_nat") %>%
    select(COMID, seg_id_nat, subsegid, est_width_m, slope, slope_len_wtd_mean, 
           lengthkm, min_elev_m, max_elev_m, seg_elev, seg_slope, seg_width, seg_width_max,
           seg_elev_min)

  return(nhd_static_inputs)

}


#' @title Combine NHD-scale static and dynamic input drivers
#' 
#' @description 
#' Function to combine river-dl input drivers for NHDPlusv2 reaches, including
#' mean width, reach slope, reach elevation, and meteorological driver data.
#' 
#' @param nhd_static_inputs data frame containing NHDPlusv2 static input data.
#' Must contain columns "COMID", "date", and "seg_id_nat".
#' @param climate_inputs data frame containing daily meteorological data to join
#' with the NHDPlusv2 static attributes. Must contain columns "COMID" and "date".
#' @param prms_dynamic_inputs data frame containing daily dynamic inputs from the
#' PRMS-SNTemp model. Must include columns "date" and "seg_id_nat".
#' @param earliest_date character string with format "YYYY-MM-DD" that indicates
#' the earliest desired date for returned NHD-scale input drivers.
#' @param latest_date character string with format "YYYY-MM-DD" that indicates
#' the most recent desired date for returned NHD-scale input drivers. 
#'
#' @returns 
#' Returns a data frame with one row per COMID in `nhd_static_inputs` and unique
#' time step in `climate_inputs`. Columns indicate the NHD and NHM segment 
#' identifiers and the static and dynamic input drivers associated with that 
#' segment and time step.
#' 
combine_nhd_input_drivers <- function(nhd_static_inputs, climate_inputs, prms_dynamic_inputs,
                                      earliest_date, latest_date){

  message("Combining NHD static inputs with dynamic climate and PRMS-SNTemp drivers...")
  
  # Combine dynamic meteorological inputs with static input data
  nhd_all_inputs <- nhd_static_inputs %>%
    left_join(y = mutate(climate_inputs, COMID = as.character(COMID)), 
              by = "COMID") %>%
    left_join(y = prms_dynamic_inputs, by = c("seg_id_nat","date")) %>%
    filter(date >= earliest_date, date <= latest_date) %>%
    relocate(seg_id_nat, .after = COMID) %>%
    relocate(subsegid, .after = seg_id_nat) %>%
    relocate(date, .after = subsegid) %>%
    mutate(seg_id_nat = as.integer(seg_id_nat))
  
  return(nhd_all_inputs)
  
}


