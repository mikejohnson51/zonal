#' Aggregate data between zones
#' Provides the ability to take attributes from one scale of polygon and split aggregate them to another.
#' @param data tabular data reference to the smaller unit
#' @param geom sf, sfc, SpatialPolygonsDataFrame, or SpatialPolygons object with polygonal geometries
#' @param crosswalk a crosswalk of smaller unit to major unit relations
#' @param ID the grouping ID
#' @param fun an optional function or character vector, as described below
#' @return data.frame or sf object
#' @details the cross walk table must have at least 4 columns containing the ID and areas of the small units and
#' and the ID and areas of the large unit. The name of the large unit IDs is provided by the `ID` parameter in the function
#' signature. The smaller unit IDs must have a corresponding attribute in the data input. The area of the larger units must be called 
#' `areasqkm`, while the area of the smaller units must be called `s_areasqkm.`
#' @export

aggregate_zones = function(data, geom, crosswalk, ID = "divide_id", fun = "mean" ){
  
  vars = names(data)
  vars = vars[!vars %in% names(crosswalk)]
  
  join_id = names(crosswalk)
  join_id = join_id[!join_id %in% c('areasqkm', 's_areasqkm', ID)]
  join_id = join_id[join_id %in% names(data)]
  
  
  dt = left_join(crosswalk, data, relationship = "many-to-many", by = join_id) %>% 
    distinct() %>% 
    mutate(coverage_fraction = pmin(1, s_areasqkm / areasqkm)) 
  
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
    exe = suppressWarnings({
      collap(dt, 
             by = as.formula(paste0("~", ID)), 
             FUN = fun, 
             keep.w = FALSE,
             w = ~coverage_fraction, 
             na.rm = TRUE)
    })
  } else {
    cols <- names(dt)[!names(dt) %in% c(ID, 'coverage_fraction')]
    if(is.null(unlist(q))){
      exe  <- dt[, lapply(.SD, FUN = fun, coverage_fraction = coverage_fraction), by = ID, .SDcols = cols]
    } else {
      exe  <- dt[, lapply(.SD, FUN = fun, coverage_fraction = coverage_fraction, unlist(q)), by = ID, .SDcols = cols]
    }
  }
  
  left_join(geom, select(exe, !!ID, any_of(vars)), by = ID)
}



