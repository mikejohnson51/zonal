#' Build Vector based Grid of input
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @return
#' @export
#' @examples
#' @importFrom dplyr mutate
#' @importFrom spex qm_rasterToPolygons
#' @importFrom raster crop raster stack
#' @importFrom sf st_crs st_transform

build_grid           = function(file, geom){
  
  r = suppressWarnings(raster::raster(file) )
  
  y.dim = dim(r)[1]
  x.dim = dim(r)[2]
  
  cols <- rows <-  r
  cols[]      = (rep(1:y.dim, each = x.dim))
  names(cols) = 'Y'
  rows[]      = (rep(1:x.dim, times = y.dim))
  names(rows) = 'X'
  
  raster::stack(cols, rows) %>% 
    raster::crop(sf::st_transform(geom, sf::st_crs(r)), snap = "out") %>% 
    spex::qm_rasterToPolygons(na.rm = FALSE) %>% 
    dplyr::mutate(grid_id = 1:n())

}
