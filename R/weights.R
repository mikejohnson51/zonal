#' Build Weighting Grid
#' @description  Returns a data.table with columns for ID, grid_id, X, Y and weight. By default this object is
#' sorted on the grid_id
#' @param data file path or 
#' @param geom sf, sfc, SpatialPolygonsDataFrame, or SpatialPolygons object with polygonal geometries
#' @param ID the name of the column providing the unique identified of each geom
#' @param progress if TRUE, display a progress bar during processing
#' @return a list(data.table, vector)
#' @export

weight_grid = function (data, geom, ID, progress = TRUE) {
  
  data = prep_input(data, subds = NULL)
  
  w = suppressWarnings({
    rbindlist(exact_extract(data[[1]], 
                            geom, 
                            fun = NULL, 
                            include_cols = ID, 
                            progress = progress,
                            include_cell = TRUE))
  })
  
  w$value = NULL
  
  w
}

#' Use Weight Grid to extract SpatRast Data
#' @description  Returns a data.table with raster data attached to weight grid
#' @param data SpatRaster or file path
#' @param w weight grid
#' @param subds subdatasets to extract
#' @return data.table
#' @export

weight_grid_to_data = function(data, w, subds = NULL){ 
  
  data = prep_input(data, subds = subds)
  
  xy <- xyFromCell(data, w$cell)
  
  cbind(w, extract(data, xy))
}

#' Execute Zonal Stats by Weight Grid
#' @description  execute
#' @param data SpatRaster or file path
#' @param w weight grid
#' @param ID the grouping ID 
#' @param fun summarization function
#' @param subds subdatasets to extract
#' @param na.rm should NA values be removed?
#' @param extra extra arguments to be passed to fun
#' @return data.table
#' @export

zone_by_weights = function(data, w, ID, fun = "mean", subds = NULL, na.rm = TRUE, extra = NULL){
  
  .SD <- coverage_fraction <- NULL
  
  dt = weight_grid_to_data(data, w, subds = subds)
  
  collapse = FALSE
  
  if(inherits(fun, "function")){
    fun = fun
  } else if(fun %in% weight_functions()$base){
    collapse = TRUE
    fun = weight_functions()$collapse[which(fun == weight_functions()$base)]
  } else {
    fun = fun
  }
  
  if(collapse){
    exe = collap(dt, 
                 by = as.formula(paste0("~", ID)), 
                 wFUN = fun, 
                 w = ~coverage_fraction, 
                 na.rm = TRUE)
  } else {
    
    cols = names(dt)[!names(dt) %in% c(ID, 'cell', 'coverage_fraction')]
    
    if("coverage_fraction" %in% formalArgs(fun)){
      exe <- dt[, lapply(.SD, FUN = fun, coverage_fraction = coverage_fraction, unlist(q)), by = ID, .SDcols = cols]
    } else {
      exe <- dt[, lapply(.SD, FUN = fun, unlist(q)), by = ID, .SDcols = cols]
    }
  }
  
  sanitize(exe)
  
  }
