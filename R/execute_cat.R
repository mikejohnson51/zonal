#' Execute Spatial Intersection on Categorical Data
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @param w a precomputed weighting grid produced with `weighting_grid`
#' @param rcl a data.frame with columns names `from` and `to`. 
#' `From` should be the categorical values used in the raster, 
#' while `to` should be the categorical names to use in the output table headings.
#' If left NULL (default) the headings will not be altered.
#' @return a data.table
#' @export
#' @importFrom raster stack ncell xyFromCell cellFromRowCol crop extent
#' @importFrom data.table data.table setnames setkey

execute_zonal_cat    = function(file = NULL, geom = NULL, ID = NULL, w = NULL, rcl = NULL){
  
  . <- frac_total <- NULL

  w       = .find_w(file, geom, ID, w)
  w_names = c("grid_id", "w", "X", "Y")
  ID = names(w)[!names(w) %in% w_names]

  cat_dt = .zonal_io(file, w)

  #TODO: leave this so the date 
  cols = names(cat_dt)[!names(cat_dt) %in% c(ID, w_names)]
  
  cat_dt[, frac_total := (w / sum(w, na.rm = TRUE)), by = c(ID)]
  cat_dt = cat_dt[, .(freq = sum(frac_total, na.rm = TRUE)), by = c(ID, cols)]

  if(!is.null(rcl)){ cat_dt[[cols]] = rcl$to[match(cat_dt[[cols]], rcl$from)] }
  
  setnames(cat_dt, c(ID, "value", "percentage"))
}


