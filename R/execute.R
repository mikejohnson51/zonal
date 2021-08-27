.getExtension <- function(file){ 
  ext <- strsplit(basename(file), split="\\.")[[1]]
  return(ext[length(ext)])
} 

#' Execute Spatial Intersection
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @param w a precomputed weighting grid produced with `weighting_grid`,
#' @param FUN an optional function or character vector, as described below
#' @return data.table
#' @export
#' @importFrom data.table getDTthreads setDTthreads

execute_zonal    = function(file = NULL, 
                            geom = NULL, 
                            ID = NULL,
                            FUN = "mean",
                            w = NULL) {
  .SD <-  NULL
  
  w = .find_w(file, geom, ID, w)
  w_names = c("grid_id", "w", "X", "Y")
  ID = names(w)[!names(w) %in% w_names]

  dt = .zonal_io(file, w)
  
  cols = names(dt)[!names(dt) %in% c(ID, w_names)]

  if(FUN == 'mean'){
    FUN = function(x, w) { sum((x * w), na.rm = TRUE)  / sum(w, na.rm = TRUE)}
  }
  
  if(FUN == "max"){
    FUN = function(x, w) { suppressWarnings(max(x, na.rm = TRUE)) }
  }
  
  if(FUN == "min"){
    FUN = function(x, w) { suppressWarnings(min(x, na.rm = TRUE)) }
  }
  
  

  threds = getDTthreads()
  
  setDTthreads(0)
    dt2 = dt[, lapply(.SD, FUN = FUN, w = w), keyby = eval(ID), .SDcols = cols]
  setDTthreads(threds)
}


