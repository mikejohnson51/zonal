#' Internal Exectute Single Summary
#' @param file SpatRast object
#' @param w weight file
#' @param w_name weight names
#' @param name variable name
#' @param FUN summary function
#' @param rcl reclassification matrix
#' @param ID geometry ID
#' @return data.table

single_file_execute <- function(file, w, FUN, rcl, ID = NULL) {
  
  
  dt <- .zonal_io(file,  w = w)
  
  if(is.null(ID)){
    ID = names(w)[ !names(w) %in% c('cell', 'coverage_fraction') ]
  }
  
  cols = names(dt)[!names(dt) %in% names(w)]

  if (FUN == "freq") {
    
    dt = dt[, frac_total := (coverage_fraction / sum(coverage_fraction, na.rm = TRUE)), by = ID]
    dt = dt[, .(freq = sum(frac_total, na.rm = TRUE)), by = c(ID, cols)]

    if (!is.null(rcl)) {
      dt[[cols]] <- rcl$to[match(dt[[cols]], rcl$from)]
    }

    exe <- setnames(dt, c(ID, "value", "percentage"))
    return(exe)
    
   } else  if (FUN == "gm_mean") {
      FUN <- function(x, coverage_fraction) {
        exp(mean(log(pmax(x, 0) * coverage_fraction), na.rm = TRUE))
      }
    } else if(FUN == "mean"){
      FUN <- function(x, coverage_fraction) {
        mean(x * coverage_fraction, na.rm  = TRUE)
      }
    } else {
      stop("FUN not valid...")
    }

    exe <- dt[, lapply(.SD, FUN = FUN, coverage_fraction = coverage_fraction), by = ID, .SDcols = cols]
    exe <- setnames(exe, c(ID, cols))
    exe
}

.getExtension <- function(file) {
  ext <- strsplit(basename(file), split = "\\.")[[1]]
  return(ext[length(ext)])
}

.find_w <- function(file, geom, ID, w) {
  if (!is.null(w) & !is.null(geom)) {
    stop("Provide either a geom and ID, OR, a precomputed weight grid (w)")
  }

  if (!is.null(geom) & is.null(ID)) {
    stop("ID is needed for geom")
  }

  if (is.null(w)) {
    w <- weighting_grid(file, geom, ID)
  }

  w
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

execute_zonal <- function(file = NULL,
                          geom = NULL,
                          ID = NULL,
                          FUN = "mean",
                          w = NULL,
                          rcl = NULL,
                          join = TRUE) {
  
  if (join & is.null(geom)) { join <- FALSE }

  if (inherits(file, "list")) {
    tmp = lapply(file, rast)
    # are they all the same?
    if (any(
      length(unique(unlist(lapply(tmp, nrow))))  > 1,
      length(unique(unlist(lapply(tmp, ncol))))  > 1,
      length(unique(unlist(lapply(tmp, ncell)))) > 1,
      length(unique(unlist(lapply(tmp, xmin))))  > 1,
      length(unique(unlist(lapply(tmp, ymin))))  > 1
    )) {
      stop("list elements have different spatial properites: \nTry: `lapply`",
        call. = FALSE
      )
    }
    
    if(all(sapply(file, inherits, "SpatRaster")) ){
      input = rast(file)
    } else {
      input = tmp
    }
  } else {
    if(inherits(file, "character")){
      input = rast(file)
    }
  }
      
  ee = c("min", "max", "count", "sum", "mean", "median", "quantile", "mode", 
         "majority", "minority", "variety", "variance", "stdev", "coefficient_of_variation",
         "weighted_mean", 'weighted_sum')
    

  if(FUN %in% ee & !is.null(geom)){
  
    suppressWarnings({
      out     = exact_extract(input, 
                              geom, 
                              fun = FUN,
                              progress = FALSE,
                              force_df = TRUE)
    })

      if(join){
        return(cbind(AOI, out))
      } else {
        out = cbind(AOI[[ID]], out)
        names(out) = c(ID, n) 
        return(out)
      }
  } else {
    
    w <- .find_w(file, geom, ID, w)
    
    file = list(file)
    
    out <- lapply(
        1:length(file),
        function(x) {
          single_file_execute(
            file = file[[x]],
            w = w,
            FUN = FUN,
            rcl = rcl,
            ID = ID
          )
        }
      )
    
    exe <- Reduce(function(...) merge(..., all = TRUE, by = ID), out)
    
    if (join) {
      merge(geom, exe, by = ID)
    } else {
      exe
    }
  }

}
