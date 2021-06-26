#' Build Weighting Grid
#' Returns a data.table with columns for ID, grid_id, X, Y and weight. By default this object is 
#' sorted on the grid_id
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @param progress show progress bar for intersection (default = TRUE)
#' @return a data.tabel
#' @export
#' @importFrom raster raster crop stack ncell addLayer
#' @importFrom exactextractr exact_extract
#' @importFrom sf st_transform st_crs st_drop_geometry st_area
#' @importFrom dplyr mutate rename `%>%`
#' @importFrom data.table setDT rbindlist
#' @importFrom rlang `:=`


weighting_grid = function(file, geom, ID, progress = FALSE){
  
  coverage_fraction = NULL
  r    = suppressWarnings({ raster::raster(file) })
  
  y.dim = dim(r)[1]
  x.dim = dim(r)[2]
  
  cols <- rows <-  r
  cols[]      = (rep(1:y.dim, each = x.dim))
  names(cols) = 'Y'
  rows[]      = (rep(1:x.dim, times = y.dim))
  names(rows) = 'X'
  
  s = raster::stack(cols, rows) %>% 
    raster::crop(sf::st_transform(geom, sf::st_crs(r)), snap = "out")
  
  cells = s[[1]]
  names(cells) = 'grid_id'
  cells[] = 1:raster::ncell(cells)
  s = raster::addLayer(s, cells)
  
  
  out1 = suppressWarnings({
    exactextractr::exact_extract(s, geom, progress = progress)
  })
  
  out2 = data.table::rbindlist(out1) %>% 
    dplyr::mutate(!!ID := rep(geom[[ID]], times = sapply(out1, nrow))) %>% 
    dplyr::rename(w = coverage_fraction)
  
  data.table::setDT(out2, key = "grid_id")
}
