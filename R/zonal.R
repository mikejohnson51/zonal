#' Sanitize Column Inclusions
#' @param exe executed zonal product
#' @return data.table
#' @export

sanitize = function(exe){
  if("cell" %in% names(exe)){ exe$cell = NULL }
  
  if("coverage_fraction" %in% names(exe)){ exe$coverage_fraction = NULL }
  
  exe
}



#' Prep Incoming Data
#' @param data SpatRaster, file path, raster
#' @param subds subdatasets
#' @return SpatRaster
#' @export
prep_input = function(data, subds = NULL){
  if(!inherits(data, "SpatRaster")) { data <- rast(data) }
  if(!is.null(subds)){ data = data[[subds]] }
  data
}

#' Execute Zonal Stats
#'
#' @param data SpatRaster or file path
#' @param geom sf, sfc, SpatialPolygonsDataFrame, or SpatialPolygons object with polygonal geometries
#' @param w a weight grid (produced with weight_grid)
#' @param ID the grouping ID
#' @param fun an optional function or character vector, as described below
#' @param subds character or boolean to select a sub-dataset. If NULL all are selected
#' @param na.rm remove na values?
#' @param progress if TRUE, display a progress bar during processing
#' @param join if TRUE the AOI will be joined to the results
#' @return sf object or data.table
#' @export

execute_zonal = function(data = NULL, 
                         geom = NULL, 
                         w = NULL, 
                         ID = NULL, 
                         fun = "mean", 
                         subds = NULL, 
                         na.rm = TRUE, 
                         progress = FALSE,
                         join = TRUE){
  
  if(is.null(data)){ stop("`data` cannot be left NULL", call. = TRUE) }
  if(is.null(ID)){ stop("`ID` cannot be left NULL", call. = TRUE) }
  if(all(is.null(AOI), is.null(w))){ stop("`AOI` and `w` cannot both be NULL", call. = TRUE) }
  
  if(all(!is.null(AOI), !is.null(w))){ 
    message("Both `AOI` and `w` provided, prioritizing `w`") 
  }
  
  if(is.null(w)){
    exe = zone_by_ee(data = data, 
                     geom = geom, 
                     ID = ID, 
                     fun = fun, 
                     subds = subds, 
                     na.rm = na.rm, 
                     progress = progress)
  } else {
    exe = zone_by_weights(data, 
                          w, ID, fun = "mean", subds = NULL, na.rm = TRUE)
  }
  
  if(join & !is.null(geom)){
    exe = merge(geom, exe, by = ID)
  }
  
  sanitize(exe)
  
}
