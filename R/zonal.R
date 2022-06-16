#' Sanitize Column Inclusions
#' @param exe executed zonal product
#' @param drop colnames to drop from table
#' @return data.table
#' @export

sanitize = function(exe, drop = NULL){
  
  drop = c(drop, "cell", "coverage_fraction")
  drop = drop[drop %in% names(exe)]
  
  if(length(drop) > 0){
    for(d in drop){
      exe[[d]] = NULL 
    }
  }
  
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
#' @param drop colnames to drop from table
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
                         join = TRUE,
                         drop = NULL){
  
  if(is.null(data)){ stop("`data` cannot be left NULL", call. = TRUE) }
  if(is.null(ID)){ stop("`ID` cannot be left NULL", call. = TRUE) }
  if(all(is.null(geom), is.null(w))){ stop("`geom` and `w` cannot both be NULL", call. = TRUE) }
  
  if(all(!is.null(geom), !is.null(w))){ 
    message("Both `geom` and `w` provided, prioritizing `w`") 
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
    exe = zone_by_weights(data = data, 
                          w = w, 
                          ID = ID, 
                          fun = fun, 
                          subds = subds, 
                          na.rm = na.rm)
  }
  
  if(join & !is.null(geom)){
    exe = merge(geom, exe, by = ID)
  }
  
  sanitize(exe, drop = drop)
  
}
