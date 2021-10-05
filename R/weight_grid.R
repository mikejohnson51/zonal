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
#' @importFrom terra rast ext project vect crs crop cells `values<-` colFromCell rowFromCell
#' @importFrom exactextractr exact_extract
#' @importFrom data.table setDT rbindlist setnames

weighting_grid = function(file, geom, ID){

  r = if(grepl("raster", class(file), ignore.case = TRUE)){
    file[[1]]
  } else {
    suppressWarnings({ rast(file)[[1]] })
  }
  
  ext = ext(project(vect(geom), terra::crs(r)))
  
  Y <- X <- grid_cells <- crop(r, ext, snap = "out")

  cells = cells(r, ext(Y))
  values(Y) = colFromCell(r, cells)
  values(X) = rowFromCell(r, cells)
  values(grid_cells) = 1:length(cells)
  
  s = c(X, Y, grid_cells)
  names(s) = c("X", "Y", "grid_id")

  out1 = suppressWarnings({ exactextractr::exact_extract(raster::stack(s),
                                                         geom,
                                                         progress = FALSE,
                                                         include_cols = ID) })

  out2 = rbindlist(out1)
  setnames(out2, "coverage_fraction", "w")
  setDT(out2, key = "grid_id")
}

