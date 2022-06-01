#' Internal Exectute Single Summary
#' @param file SpatRast object
#' @param w weight file
#' @param w_name weight names
#' @param name variable name
#' @param FUN summary function
#' @param rcl reclassification matrix
#' @param ID geometry ID
#' @return data.table

single_file_execute <- function(file, w, FUN, rcl, ID = NULL, area_weight = TRUE) {
  
  dt <- .zonal_io(file,  w = w)

  if(is.null(ID)){
    ID = names(w)[ !names(w) %in% c('cell', 'coverage_fraction', "rID", "gID") ]
  }
  
  cols = names(dt)[!names(dt) %in% c('cell', 'coverage_fraction', names(w))]
  
  if(inherits(FUN, "function")){
    FUN = FUN
  } else if (FUN == "freq") {
    
    dt = dt[, frac_total := (coverage_fraction / sum(coverage_fraction, na.rm = TRUE)), by = ID]
    dt = dt[, .(freq = sum(frac_total, na.rm = TRUE)), by = c(ID, cols)]

    if (!is.null(rcl)) {
      dt[[cols]] <- rcl$to[match(dt[[cols]], rcl$from)]
    }

    exe <- setnames(dt, c(ID, "value", "percentage"))
    
    return(exe)
    
   } else  if (FUN == "gm_mean") {
      FUN <- function(x, coverage_fraction) {
        # https://www.wwdmag.com/channel/casestudies/handling-zeros-geometric-mean-calculation
        # replace negative numbers with 0
        x = pmax(x, 0)
        
        # If 0 is in set, add 1 and set FLAG
        if(any(x == 0)){
          add1 = TRUE
          x = x + 1
        } else {
          add1 = FALSE
        }
        
        if(add1){
          exp(mean(log(x * coverage_fraction), na.rm = TRUE)) - 1 
        } else {
          exp(mean(log(x * coverage_fraction), na.rm = TRUE))
        }
        
      }
    } else if(FUN == "mean"){
      FUN <- function(x, coverage_fraction) {
        mean(x * coverage_fraction, na.rm  = TRUE)
      }
    } else if (FUN == "mode") {
      area_weight = FALSE
      FUN <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
    } else if(FUN == "circular_mean") {
      area_weight = FALSE
      
      FUN <- function (x) {
    
        degrad = pi / 180 
        
        sinr <- sum(sin(x * degrad), na.rm = T)
        
        cosr <- sum(cos(x * degrad), na.rm = T)
        
        val = atan2(sinr, cosr) * (1 / degrad)
        
        ifelse(val < 0, 180 + (val + 180), val)
      }
    } else {
      stop("FUN not valid...")
    }

  
  if("coverage_fraction" %in% formalArgs(FUN)){
    exe <- dt[, lapply(.SD, FUN = FUN, coverage_fraction = coverage_fraction), by = ID, .SDcols = cols]
  } else if(area_weight){
    exe <- dt[, lapply(.SD, FUN = function(x){FUN(na.omit(x * coverage_fraction)) }), by = ID, .SDcols = cols]
  } else {
    exe <- dt[, lapply(.SD, FUN = FUN), by = ID, .SDcols = cols]
  }
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
                          join = TRUE, 
                          area_weight = TRUE) {
  
  if (join & is.null(geom)) { join <- FALSE }

  if(inherits(file, "SpatRaster")){
    input = file
  } else if (inherits(file, "list")) {
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
    
  
  pure_ee = FALSE
  
  if(inherits(FUN, "function")){
    pure_ee = TRUE
  } else {
    pure_ee = FUN %in% ee
  }
  

  if(pure_ee & !is.null(geom)){
  
    suppressWarnings({
      out     = exact_extract(input, 
                              geom, 
                              fun = FUN,
                              progress = FALSE,
                              force_df = TRUE)
    })

      if(join){
        return(cbind(geom, out))
      } else {
        out = cbind(geom[[ID]], out)
        names(out) = c(ID, names(out)[-1])
        return(out)
      }
    
  } else {
    
    w <- .find_w(input, geom, ID, w)
   
    exe <- single_file_execute(
            file = input,
            w = w,
            FUN = FUN,
            rcl = rcl,
            ID = ID,
            area_weight = area_weight
          )
    
    if (join) {
      merge(geom, exe, by = ID)
    } else {
      exe
    }
  }

}
