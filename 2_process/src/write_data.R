#' @title Save data frames as a feather file
#' 
#' @param data data frame object to save as a feather file
#' @param file_out character string indicating the name of the output file,
#' including file path and .feather extension
#' 
write_feather <- function(data, file_out){
  
  # save data as a feather file
  arrow::write_feather(data, file_out)
  
  # return the file name
  return(file_out)
  
}



#' @title Write an R data frame to a zarr data store
#'
#' @description 
#' This function uses {reticulate} to write an R data frame to a Zarr data
#' store (the file format river-dl currently takes). write_df_to_zarr()
#' originally written by J. Sadler in https://github.com/USGS-R/drb-do-ml
#' and modified for use in drb-gw-hw-model-prep.
#'
#' @param df a data frame of data. Must contain column "COMID".
#' @param index vector of strings indicating which column(s) should be the index
#' @param out_zarr character string indicating the name of the saved file, 
#' including file path and extension.
#'
#' @returns 
#' Returns the file path of the saved zarr file
#' 
write_df_to_zarr <- function(df, index_cols, out_zarr) {
  
  
  # convert to a python (pandas) DataFrame so we have access to the object methods (set_index and to_xarray)
  py_df <- reticulate::r_to_py(df)
  pd <- reticulate::import("pandas")
  py_df[["date"]] = pd$to_datetime(py_df$date)
  py_df[["COMID"]] = py_df$COMID$astype("str")
  
  
  # set the index so that when we convert to an xarray dataset it is indexed properly
  py_df  <- py_df$set_index(index_cols)
  
  # convert to an xarray dataset
  ds <- py_df$to_xarray()
  ds$to_zarr(out_zarr, mode = 'w')
  
  return(out_zarr)
  
}


