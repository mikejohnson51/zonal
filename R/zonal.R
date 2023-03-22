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
#' @param area_weight should an area weighted summary be used?
#' @param ... optional arguments to pass fun
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
                         drop = NULL,
                         area_weight = TRUE,
                         ...){
  
  .SD <- . <- coverage_fraction <- NULL
  args = as.list(match.call.defaults()[-1]) 
  q = args[names(args) %in% formalArgs(fun)]


  if(is.null(data)){ stop("`data` cannot be left NULL", call. = TRUE) }
  if(is.null(ID)){ stop("`ID` cannot be left NULL", call. = TRUE) }
  if(all(is.null(geom), is.null(w))){ stop("`geom` and `w` cannot both be NULL", call. = TRUE) }
  
  if(all(!is.null(geom), !is.null(w))){ 
    message("Both `geom` and `w` provided, prioritizing `w`") 
  }
  
  if(!area_weight){
    
    v = project(vect(geom), crs(data))
    data = crop(prep_input(data, subds = subds), v)
    
    dt = as.data.frame(c(rasterize(v, data[[1]], field = ID), data)) %>% 
      filter(complete.cases(.)) %>% 
      mutate(coverage_fraction = 1) %>% 
      data.table()
    
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
      cols <- names(dt)[!names(dt) %in% c(ID, 'coverage_fraction')]
      if(is.null(unlist(q))){
        exe  <- dt[, lapply(.SD, FUN = fun, coverage_fraction = coverage_fraction), by = ID, .SDcols = cols]
      } else {
        exe  <- dt[, lapply(.SD, FUN = fun, coverage_fraction = coverage_fraction, unlist(q)), by = ID, .SDcols = cols]
      }

    }
    
    exe = sanitize(exe)
    
  } else {
    
    if(is.null(w)){
      # exe = zone_by_ee(data = data, 
      #                  geom = geom, 
      #                  ID = ID, 
      #                  fun = fun, 
      #                  subds = subds, 
      #                  na.rm = na.rm, 
      #                  progress = progress,
      #                  extra = q)
      w = weight_grid(data, geom, ID, progress )
    } #else {
    
    exe = zone_by_weights(data = data, 
                          w = w, 
                          ID = ID, 
                          fun = fun, 
                          subds = subds, 
                          na.rm = na.rm,
                          extra = q)
    
    
  }
    
   
  #}
  
  if(join & !is.null(geom)){
    exe = merge(geom, exe, by = ID)
  }
  
  sanitize(exe, drop = drop)
  
}
