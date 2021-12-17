#' Execute Spatial Intersection on Categorical Data
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @param w a precomputed weighting grid produced with `weighting_grid`
#' @param rcl a data.frame with columns names `from` and `to`. 
#' `From` should be the categorical values used in the raster, 
#' while `to` should be the categorical names to use in the output table headings.
#' If left NULL (default) the headings will not be altered.
#' @param join should output be joined to input geom by the supplied ID (geom cannot be NULL)
#' @return a data.table
#' @export
#' @importFrom data.table setnames setkey `:=`

execute_zonal_cat    = function(file = NULL, 
                                geom = NULL, 
                                ID   = NULL, 
                                w    = NULL, 
                                rcl  = NULL,
                                join = TRUE){
  
  . <- frac_total <- NULL
  
  if(join & is.null(geom)){
    warning("Can't `join` to NULL geom")
    join = FALSE
  }

  w       = .find_w(file, geom, ID, w)
  w_names = c("grid_id", "w")
  ID      = names(w$weight_map)[!names(w$weight_map) %in% w_names]

  cat_dt = .zonal_io(file, w)

  cols = names(cat_dt)[!names(cat_dt) %in% c(ID, w_names)]
  
  cat_dt[, frac_total := (w / sum(w, na.rm = TRUE)), by = c(ID)]
  cat_dt = cat_dt[, .(freq = sum(frac_total, na.rm = TRUE)), by = c(ID, cols)]

  if(!is.null(rcl)){ cat_dt[[cols]] = rcl$to[match(cat_dt[[cols]], rcl$from)] }
  
  exe = setnames(cat_dt, c(ID, "value", "percentage"))
  
  
  if(join){
    merge(geom, exe, by = ID)
  } else {
    exe
  }
}


