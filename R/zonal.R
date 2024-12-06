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
#' @param subds positive integer or character to select a sub-dataset. If zero or "", all sub-datasets are returned (if possible)
#' @param lyrs positive integer or character to select a subset of layers (a.k.a. "bands")
#' @param win SpatExtent to set a window (area of interest)
#' @return SpatRaster
#' @export

prep_input = function(data, subds = 0, lyrs = NULL, win = NULL){
  
  if(inherits(data, "SpatRaster")) {
    if(is.null(lyrs) & subds == 0){ lyrs = names(data) }
     
    s = sources(data)
    
    if(all(s == "")){
      r = data
    } else {
      r = rast(s, win = win, lyrs = lyrs, snap = "out")
    }
  }
  
  if(subds != 0){ r = r[[subds]] }
  
  r
}

#' Prep Incoming Data
#' @param geom sf or SpatVector object
#' @param crs coordinate reference system
#' @return SpatVector
#' @export

prep_geom = function(geom, crs = NULL){
  if(!inherits(geom, "SpatVector")) { geom <- vect(geom) }
  if(!is.null(crs)){ geom =  project(geom, crs) }
  geom
}


#' Execute Zonal Stats
#'
#' @param data SpatRaster or file path
#' @param geom sf, sfc object with polygonal geometries
#' @param w a weight grid (produced with weight_grid)
#' @param ID the grouping ID
#' @param fun an optional function or character vector, as described below
#' @param subds character or boolean to select a sub-dataset. If NULL all are selected
#' @param progress if TRUE, display a progress bar during processing
#' @param join if TRUE the geom will be joined to the results
#' @param drop colnames to drop from table
#' @param ... optional arguments to pass fun
#' @return sf object or data.table
#' @export

execute_zonal = function(data = NULL, 
                         geom = NULL, 
                         w = NULL, 
                         ID = NULL, 
                         fun = "mean", 
                         subds = 0, 
                         progress = FALSE,
                         join = TRUE,
                         drop = NULL,
                         ...){
  
  .SD <- . <- coverage_fraction <- NULL
  
  if(!is.list(data)){ data =list(data) }
  exe = list()

  if(is.null(data)){ stop("`data` cannot be left NULL", call. = TRUE) }
  if(is.null(ID)){ stop("`ID` cannot be left NULL", call. = TRUE) }
  if(all(is.null(geom), is.null(w))){ stop("`geom` and `w` cannot both be NULL", call. = TRUE) }
  
  if(all(!is.null(geom), !is.null(w))){ 
    message("Both `geom` and `w` provided, prioritizing `w`") 
  }
  
  n = sapply(data, nlyr)
  
  if(inherits(fun, "function") & any(n > 5)){
    ee = FALSE
  } else if(inherits(fun, "function") & any(n <= 5)){
    ee = TRUE
  } else if(fun %in% ee_functions()){
    ee = TRUE
  }
  
  if(!is.null(w)){ ee = FALSE }
  
  if(ee){
    
    for(i in 1:length(data)){

      exe[[i]] = zone_by_ee(data     = data[[i]],
                            geom     = geom,
                            ID       = ID,
                            fun      = fun,
                            subds    = subds,
                            progress = progress,
                            join     = FALSE,
                            ...
                            )
    }
  
    
  } else {
    
    if(is.null(w)){ 
      d = prep_input(data[[1]], subds = 0, win = ext(project(vect(geom), crs(data[[1]]))))
      w = weight_grid(d, geom, ID, progress) 
    } else {
      d = data[[1]]
    }
    
    exe[[1]] = zone_by_weights(data = d, 
                               w    = w, 
                               ID   = ID, 
                               fun  = fun)
   
  }
    
  if(join & !is.null(geom)){ exe = merge(geom, exe, by = ID) }
  
  for(i in 1:length(exe)){
    exe[[i]] = sanitize(exe[[i]], drop = drop)
  }
 
  if(length(exe) == 1){
    return(exe[[1]])
  } else {
    return(exe)
  }
  
}

#' Extract and Transform Time Series Data
#'
#' This function processes an input dataset by reshaping it from wide to long format, separating columns into descriptions and dates, 
#' and renaming the resulting dataset for further analysis.
#'
#' @param output A data frame containing the time series data in wide format. It must have a column with the name matching `ID` and other columns 
#'   with names in the format "description_Date" (e.g., "temp_2024-01-01").
#' @param ID A character string specifying the column name that uniquely identifies rows in the dataset.
#'
#' @return A transformed data frame with the following changes:
#' \describe{
#'   \item{description}{The variable names extracted from the original column names.}
#'   \item{Date}{A `Date` column converted from the original column names.}
#'   \item{value}{The associated values for each combination of `ID`, `description`, and `Date`.}
#' }
#' @importFrom tidyr pivot_longer separate
#' @importFrom dplyr mutate rename
#' @export

ts_extract = function(output, ID){
  
  name <- Date <- value <- NULL
  
  xx = pivot_longer(output, -ID) |>
    separate(name, c("description", "Date"), sep = "_") |>
    mutate(Date = as.Date(Date))
  
  rename(xx, !!xx$description[1] := value)
  
}
