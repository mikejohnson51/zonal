.find_w = function(file, geom, ID, w){
  
  if(!is.null(w) & !is.null(geom)){
    stop("Provide either a geom and ID, OR, a precomputed weight grid (w)")
  }
  
  if(!is.null(geom) & is.null(ID)){
    stop("ID is needed for geom")
  }
  
  if(is.null(w)){ w = weighting_grid(file, geom, ID) }
  
  setkey(w, "grid_id")
}

#' Build Weighting Grid
#' @description  Returns a data.table with columns for ID, grid_id, X, Y and weight. By default this object is 
#' sorted on the grid_id
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units 
#' @param ID the name of the column providing the unique identified of each geom
#' @return a data.table
#' @export
#' @importFrom raster stack
#' @importFrom sf sf_use_s2 st_as_sfc st_bbox st_crs st_set_crs st_transform st_intersection st_cast
#' @importFrom terra rast ext vect rowColFromCell crop values ncell
#' @importFrom exactextractr exact_extract
#' @importFrom data.table setDT rbindlist setnames `:=`

weighting_grid = function(file, geom, ID){

  
  sf::sf_use_s2(FALSE)
  
  r = if(grepl("raster", class(file), ignore.case = TRUE)){
    file[[1]]
  } else {
    suppressWarnings({ terra::rast(file)[[1]] })
  }
  
  r_crs = crs(r)
  
  domain = sf::st_as_sfc(sf::st_bbox(as.vector(terra::ext(r)))) %>% 
    sf::st_set_crs(r_crs)
  
  g_bbox = sf::st_as_sfc(sf::st_bbox(geom), crs = sf::st_crs(geom)) %>% 
    sf::st_transform(r_crs) %>% 
    sf::st_intersection(domain)

  g_pts = sf::st_cast(g_bbox, "POINT")
  
  ind = terra::extract(r, vect(g_pts), cells = T)
  
  ind2 = terra::rowColFromCell(r, ind$cell)
  
  Y <- X <- grid_cells <-  terra::crop(r, vect(g_bbox), snap = "out")
  
  values(Y) = (rep(min(ind2[,1]):max(ind2[,1]), each = dim(Y)[2]))
  values(X) = (rep(min(ind2[,2]):max(ind2[,2]), times = dim(X)[1]))
  values(grid_cells) = 1:ncell(X)
  
  s = rast(list(X,Y,grid_cells))
  names(s) = c("X", "Y", "grid_id")

  out1 = suppressWarnings({ exactextractr::exact_extract(raster::stack(s),
                                                         geom,
                                                         progress = FALSE,
                                                         include_cols = ID) })

  out2 = data.table::rbindlist(out1)
  data.table::setnames(out2, "coverage_fraction", "w")
  data.table::setDT(out2, key = "grid_id")
}

