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
#' @importFrom terra rast crop ncell crs ext project vect
#' @importFrom exactextractr exact_extract
#' @importFrom data.table setDT rbindlist `:=`

weighting_grid = function(file, geom, ID){
  
  if(grepl("raster|character", class(file), ignore.case = TRUE)){ file = file[[1]] }
  
  r    = suppressWarnings({ terra::rast(file)[[1]] })
  
  y.dim = dim(r)[1]
  x.dim = dim(r)[2]
  
  cols <- rows <- r[[1]]
  cols[]      = (rep(1:y.dim, each = x.dim))
  names(cols) = 'Y'
  rows[]      = (rep(1:x.dim, times = y.dim))
  names(rows) = 'X'
  
  # We are doing this because terra snap out often crashes R!
  vect = terra::project(terra::vect(geom), terra::crs(r))
  ext  = terra::ext(vect)
  res  = terra::res(r)[1]
  ext  = c(ext$xmin, ext$xmax, ext$ymin, ext$ymax)

  st =  terra::crop(terra::rast(list(cols, rows)), ext, snap = "out")

  cells        = st[[1]]
  names(cells) = 'grid_id'
  cells[] = 1:terra::ncell(cells)
  s       = terra::rast(list(st, cells))

  out1 = suppressWarnings({ exactextractr::exact_extract(raster::stack(s), 
                                                         geom, 
                                                         progress = FALSE,
                                                         include_cols = ID) })
  
  out2 = data.table::rbindlist(out1) 
  setnames(out2, "coverage_fraction", "w")
  setDT(out2, key = "grid_id")
}
