#' Build Intersection Weights
#' Returns a data.table with columns for ID, grid_id, and weight. By default this object is 
#' sorted on the grid_id
#' @param grid the vector grid produced with `build_grid`
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @param na.rm should NAs be removed from the weight object
#' @param df should a data.frame be returned (TRUE) or a data.table (FALSE)
#' @return
#' @export
#' @importFrom sf st_transform st_crs st_drop_geometry st_area
#' @importFrom dplyr mutate right_join select
#' @importFrom data.table setDT
#' 
build_weights = function(grid, geom, ID = NULL, na.rm = TRUE, df = FALSE){
  
  if (sf::st_crs(grid) != sf::st_crs(geom)) {
    grid <- sf::st_transform(grid, sf::st_crs(geom))
  }
  
  grid_area = sf::st_area(grid[1,])
  
  if(is.null(ID)){ ID = names(geom)[1]}
  
  if(ID %in% names(geom)){ 
    geom = geom[ ,ID] 
  } else { 
    stop(paste(ID, "is not an attribute of the geom input"))
  }
  
  geom = dplyr::mutate(geom, area_geom = units::drop_units(st_area(geom)))
  
  tmp = dplyr::select(grid, grid_id) %>% 
    sf::st_intersection(geom) %>%
    dplyr::mutate(cellarea = sf::st_area(.),
                  w = as.numeric(cellarea / grid_area)) %>%
    sf::st_drop_geometry() %>%
    dplyr::right_join(grid, by = 'grid_id') %>%
    dplyr::select(grid_id, !!ID, w, X, Y)
  
  if(!df){
    tmp = setDT(tmp, key = "grid_id")
  }
  
  if(na.rm){tmp = na.omit(tmp)}
  
  tmp
  
}


