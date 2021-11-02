.getExtension <- function(file){ 
  ext <- strsplit(basename(file), split="\\.")[[1]]
  return(ext[length(ext)])
} 

.find_w = function(file, geom, ID, w) {
  if (!is.null(w) & !is.null(geom)) {
    stop("Provide either a geom and ID, OR, a precomputed weight grid (w)")
  }
  
  if (!is.null(geom) & is.null(ID)) {
    stop("ID is needed for geom")
  }
  
  if (is.null(w)) {
    w = weighting_grid(file, geom, ID)
  }
  
  w
}


#' Get Mode
#' @param x vector of values
#' @return mode value
#' @export

getmode <- function(x) {
  y <- unique(x)
  y[which.max(tabulate(match(x, y)))]
}

#' Get geometric mean
#' @description Vectorized and suited to handle NAs and negative values
#' @param x vector of values
#' @return mode value
#' @export
#' 
gm_mean = function(x, na.rm=TRUE){
  if(any(x  < 0)){
    y = x[x >= 0]
    warning('Applying geometic mean to data with negatives. Removing negatives')
  }
  
  exp(sum(log(y), na.rm=na.rm) / length(x))
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
  
  w       = .find_w(file, geom, ID, w)
  w_names = c("grid_id", "w")
  ID      = names(w$weight_map)[!names(w$weight_map) %in% w_names]

  dt = .zonal_io(file, w)

  cols = names(dt)[!names(dt) %in% c(ID, w_names)]

  if(FUN == 'mean'){
    FUN = function(x, w) { sum((x * w), na.rm = TRUE)  / sum(w, na.rm = TRUE)}
  } else if(FUN == "max"){
    FUN = function(x, w) { suppressWarnings(max(x, na.rm = TRUE)) }
  } else if(FUN == "min"){
    FUN = function(x, w) { suppressWarnings(min(x, na.rm = TRUE)) }
  } else if(FUN == "mode"){
    FUN = function(x, w) { suppressWarnings(getmode(x)) }
  } else if(FUN == "gm_mean"){
    FUN = function(x, w) { suppressWarnings(gm_mean(x)) }
  } else {
    stop("FUN not valid...")
  }
  
  threds = getDTthreads()
  setDTthreads(0)
    exe = dt[, lapply(.SD, FUN = FUN, w = w), keyby = eval(ID), .SDcols = cols]
  setDTthreads(threds)

  return(exe)
}


