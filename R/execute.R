#' Internal Exectute Single Summary
#' @param file SpatRast object
#' @param w weight file
#' @param w_name weight names
#' @param name variable name
#' @param FUN summary function
#' @param rcl reclassification matrix
#' @param ID geometry ID
#' @return data.table

single_file_execute = function(file, w, w_names, name,  FUN, rcl, ID){
  
  dt   = .zonal_io(file, w)
  cols = names(dt)[!names(dt) %in% c(ID, w_names)]
  
  if(!is.null(name)){
    names = gsub("V", paste0(name, "_"), cols)
  } else {
    names = cols
  }
  
  if(FUN == "freq"){
    dt[, frac_total := (w / sum(w, na.rm = TRUE)), by = c(ID)]
    
    dt = dt[, .(freq = sum(frac_total, na.rm = TRUE)), by = c(ID, cols)]
    
    if(!is.null(rcl)){ 
      dt[[cols]] = rcl$to[match(dt[[cols]], rcl$from)] 
    }
    
    exe = setnames(dt, c(ID, "value", "percentage"))
  } else {
    
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
    
    exe = dt[, lapply(.SD, FUN = FUN, w = w), keyby = eval(ID), .SDcols = cols]
    exe = setnames(exe, c(ID, names))
  }
  
  exe
}

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

getmode <- function(x) {
  y <- unique(x)
  y[which.max(tabulate(match(x, y)))]
}

gm_mean = function(x, na.rm=TRUE){
  if(any(x  < 0)){
    x = x[x >= 0]
    warning('Applying geometic mean to data with negatives. Removing negatives')
  }
  
  exp(sum(log(x), na.rm=na.rm) / length(x))
}

#' Execute Spatial Intersection
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @param w a precomputed weighting grid produced with `weighting_grid`,
#' @param FUN an optional function or character vector, as described in details
#' @param rcl a data.frame with columns names `from` and `to`. 
#' `From` should be the categorical values used in the raster, 
#' while `to` should be the categorical names to use in the output table headings.
#' If left NULL (default) the headings will not be altered.
#' @param join should output be joined to input geom by the supplied ID (geom cannot be NULL)
#' @return data.table or sf object
#' @export
#' @details The are current 6 built in summary functions:
#' \itemize{
#'  \item{"mean"}{The mean value of the coverage data}
#'  \item{"mode"}{The most frequent of the coverage values}
#'  \item{"max"}{The maximum of the coverage values}
#'  \item{"min"}{The minimum of the coverage values}
#'  \item{"gm_mean"}{The geometric mean of the coverage values}
#'  \item{"percent"}{The percentage of each covered value (categorical data)}
#' }
#' @importFrom data.table `:=`
#' @importFrom terra nrow ncol xmin xmax ncell

execute_zonal  = function(file = NULL, 
                          geom = NULL, 
                          ID = NULL,
                          FUN = "mean",
                          w = NULL,
                          rcl = NULL,
                          join = TRUE) {
  
  . <- .SD <- frac_total <- NULL
  
  if(join & is.null(geom)){ join = FALSE }
  
  if(inherits(file, 'list')){
    #are they all the same?
    if(any(length(unique(unlist(lapply(file, nrow)))) > 1,
           length(unique(unlist(lapply(file, ncol)))) > 1,
           length(unique(unlist(lapply(file, ncell)))) > 1,
           length(unique(unlist(lapply(file, xmin)))) > 1,
           length(unique(unlist(lapply(file, ymin)))) > 1)){
      stop('list elements have different spatial properites: \nTry: `lapply`',
           call.=FALSE)
    }
  } else {
    file = list(file)
  }
  
  w       = .find_w(file[[1]], geom, ID, w)
  #w_names = c("grid_id", "w")
  #ID      = names(w$weight_map)[!names(w$weight_map) %in% w_names]
  
  names    = names(file)

  out = lapply(1:length(file), 
         function(x){
           single_file_execute(file = file[[x]], 
                               w = w, 
                               w_names = c("grid_id", "w"), 
                               name = names[x], 
                               FUN = FUN, 
                               rcl = rcl, 
                               ID = ID)
         }
  )
  
  exe <- Reduce(function(...) merge(..., all = TRUE, by = ID), out)

  if(join){
    merge(geom, exe, by = ID)
  } else {
    exe
  }
}

