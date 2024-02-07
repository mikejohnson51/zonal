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
#' @param data SpatRaster, file path, raster
#' @param subds subdatasets
#' @return SpatRaster
#' @export

prep_geom = function(geom, crs = NULL){
  if(!inherits(geom, "SpatVecotr")) { geom <- vect(geom) }
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
#' @param na.rm remove na values?
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
      d = prep_input(data[[i]], subds = 0, win = ext(vect(geom)))
      w = weight_grid(d, geom, ID, progress) 
    } else {
      d = data
    }
    
    for(i in 1:length(d)){
    
      exe[[i]] = zone_by_weights(
                            data = d[[i]], 
                            w = w, 
                            ID = ID, 
                            fun = fun, 
                            ...
                            )
    }
    
    
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

ts_extract = function(output, ID){
  
  xx = pivot_longer(output, -ID) |>
    separate(name, c("description", "Date"), sep = "_") |>
    mutate(Date = as.Date(Date))
  
  rename(xx, !!xx$description[1] := value)
  
}
