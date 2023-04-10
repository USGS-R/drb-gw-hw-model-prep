#' @title Summarize raster values using a polygon mask.
#' 
#' @description 
#' This function takes a raster and vector polygon layer as inputs and 
#' summarizes the raster values within each polygon. Aggregation is a
#' weighted mean.
#' 
#' @param raster file path to raster file (.tif or .adf), raster or SpatRaster object.
#' @param nhd_polygon_layer polygon vector object. Polylines will not be accepted. 
#' Must buffer polylines before adding as input.
#' @param weighted_mean_col_name col name for new summarized col name
#' 
raster_in_polygon_weighted_mean <- function(raster,
                                            nhd_polygon_layer,
                                            weighted_mean_col_name,
                                            feature_id = NULL){
  
  ## SPATRASTER
  if(class(raster) != 'SpatRaster'){
    spatraster <- rast(raster)
  }else{spatraster <- raster}
  
  # CHECKS
  ## polygon_layer geometries 
  if(any(!st_is_valid(nhd_polygon_layer))){
    nhd_polygon_layer <- st_make_valid(nhd_polygon_layer)
    message('shp geometries fixed')
  } 
  
  ## Match crs
  if(!st_crs(spatraster) == st_crs(nhd_polygon_layer)){
    message('crs are different. Transforming ...')
    nhd_polygon_layer <- st_transform(nhd_polygon_layer, crs = st_crs(spatraster))
    if(st_crs(spatraster) == st_crs(nhd_polygon_layer)){
      message('crs now aligned')}
  }else{
    message('crs are already aligned')
  }
  
  # AGGREGATE
  ## Extract raster values
  raster_val_per_polygon <- terra::extract(spatraster, vect(nhd_polygon_layer),
                                           fun = mean, weighted = TRUE,
                                           na.rm = TRUE) %>%
    as.data.frame()
  
  ## Round and join to polygon layer
  nhd_polygon_layer[weighted_mean_col_name] <- round(raster_val_per_polygon[,2], 3)
  
  # RETURN
  return(nhd_polygon_layer[c(feature_id,weighted_mean_col_name)])
  
}
