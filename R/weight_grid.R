#' Build Weighting Grid
#' @description  Returns a data.table with columns for ID, grid_id, X, Y and weight. By default this object is
#' sorted on the grid_id
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units
#' @param ID the name of the column providing the unique identified of each geom
#' @return a list(data.table, vector)
#' @export
#' @importFrom terra rast ext project vect cells setValues
#' @importFrom exactextractr exact_extract
#' @importFrom data.table setDT rbindlist setnames

weighting_grid <- function(file, geom, ID) {

  r <- rast(file[[1]])[[1]]

  ext = crop(r, project(vect(geom), crs(r)), snap = "out")

  gIds <- cells(r, ext(ext))

  r <- setValues(ext, gIds)
  
  out1 = suppressWarnings({
    rbindlist(exact_extract(r,
    geom,
    fun = NULL,
    progress = FALSE,
    include_cols = ID,
    include_cell = TRUE
  ))
})
  
  out2 = setnames(out1,
           old = c("cell", "value"),
           new = c("rID", "gID")
  )
  
  out2
  
}
