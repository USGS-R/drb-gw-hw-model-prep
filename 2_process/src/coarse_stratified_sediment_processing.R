#' @title Calculate the proportion of buffered reach area has coarse stratified
#' sediments.
#' 
#' @description 
#' This function takes a vector polygon sf object that contains the areas of
#' coarse stratified sediment surface material, and calculates the proportion
#' of this area that is within the NHM reach buffer zones (250 m on either side
#' of the segment flowline).
#' 
#' @param buffered_reaches_sf sf object containing the buffered river segments
#' (buffered on either side of the segment flowline).
#' @param buffered_reaches_area_col character string indicating the column in 
#' `buffered_reaches_sf` that contains the total area (km2) of the buffered reach.
#' @param coarse_sediments_area_sf sf object containing polygons representing areas
#' of coarse stratified sediments within the Delaware River Basin. 
#' @param prms_col character string indicating the name of the column that contains
#' the segment identifier.
#' 
#' @return 
#' The output is a sf object of the buffered reaches and three calculated columns: 
#' total_reach_buffer_area_km2 = total area of buffered reach
#' cs_area_km2 = area of coarse stratified sediment within given buffered reach  
#' cs_area_proportion = proportion of coarse stratified sediment area within reach area 
#' 
coarse_sediment_area_calc <- function(buffered_reaches_sf, 
                                      buffered_reaches_area_col,
                                 coarse_sediments_area_sf,
                                 prms_col = 'PRMS_segid'){
  
  # Grab area of coarse sediment within buffer
  cs_df <- sf::st_intersection(buffered_reaches_sf,
                               coarse_sediments_area_sf) %>% 
    group_by(.data[[prms_col]]) %>% 
    dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
    mutate(cs_area_km2 = units::set_units(st_area(.), km^2)) %>% 
    ungroup() %>% 
    st_drop_geometry()
 
  # Join to final buffered reaches df 
  buffered_reaches_w_cs_df <- left_join(buffered_reaches_sf, 
                             cs_df, 
                             by = prms_col) %>%
  ## creating area cols for final df with all PRMS reaches  
  mutate(cs_area_km2 = units::set_units(ifelse(is.na(cs_area_km2), 0, cs_area_km2), km^2),
          ## calculating proportion of coarse stratified sediment within buffered reach using input area of reach col
           cs_area_proportion = round(cs_area_km2 / .data[[buffered_reaches_area_col]], 3)) %>%
    ## re order cols 
    relocate(geometry, .after = last_col())
  
  # Removing unit on proportion
  buffered_reaches_w_cs_df$cs_area_proportion <- units::drop_units(buffered_reaches_w_cs_df$cs_area_proportion)
  
  return(buffered_reaches_w_cs_df)
  
}


