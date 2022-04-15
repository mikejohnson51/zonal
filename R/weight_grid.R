#' Build Weighting Grid
#' @description  Returns a data.table with columns for ID, grid_id, X, Y and weight. By default this object is
#' sorted on the grid_id
#' @param file path to a gridded file (either .tif or .nc)
#' @param geom sf object of aggregation units
#' @param ID the name of the column providing the unique identified of each geom
#' @return a list(data.table, vector)
#' @export
#' @importFrom terra rast ext project vect xmin xmax ymin ymax colFromX rowFromY window align setValues crs cells
#' @importFrom exactextractr exact_extract
#' @importFrom data.table setDT rbindlist setnames

weighting_grid <- function(file, geom, ID) {
  #
  #   r <- rast(file[[1]])[[1]]
  #
  #   ext1 <- ext(project(vect(geom), crs(r)))
  #
  #   ext2 <- ext(c(
  #     max(xmin(ext1), xmin(r)),
  #     min(xmax(ext1), xmax(r)),
  #     max(ymin(ext1), ymin(r)),
  #     min(ymax(ext1), ymax(r))
  #   ))
  #
  #   dims <- c(
  #     xmin = colFromX(r, xmin(ext2)),
  #     xmax = colFromX(r, xmax(ext2)),
  #     ymin = rowFromY(r, ymin(ext2)),
  #     ymax = rowFromY(r, ymax(ext2))
  #   )
  #
  #   window(r) <- align(ext2, r, snap = "out")
  #
  #   #r <- setValues(r, 1:ncell(r))
  #
  #   out1 <- suppressWarnings({
  #     exact_extract(r,
  #       geom,
  #       fun = NULL,
  #       progress = FALSE,
  #       include_cols = ID,
  #       include_cell = TRUE
  #     )
  #   })
  #
  #   out2 <- rbindlist(out1)
  #
  #   setnames(out2, old = c("coverage_fraction", "cell"), new = c("w", "grid_id"))
  #   out2$value = NULL
  #   setDT(out2, key = "grid_id")
  #
  #   return(list(weight_map = out2, dims = dims))
  # }


  # ensure we have file 1
  r <- rast(file[[1]])[[1]]

  # get AOI extent in R crs
  #ext1 <- ext(project(vect(geom), crs(r)))
  
  ext = terra::crop(r, vect(geom), snap = "out")

  # "snap" AOI to R domains if needed
  # ext2 <- ext(c(
  #   max(xmin(ext1), xmin(r)),
  #   min(xmax(ext1), xmax(r)),
  #   max(ymin(ext1), ymin(r)),
  #   min(ymax(ext1), ymax(r))
  # ))

  # Get cols and rows of snapped extents
  # dims <- c(
  #   xmin = colFromX(r, xmin(ext2)),
  #   xmax = colFromX(r, xmax(ext2)),
  #   ymin = rowFromY(r, ymin(ext2)),
  #   ymax = rowFromY(r, ymax(ext2))
  # )

  #ext3 <- align(ext2, r, snap = "out")
  gIds <- cells(r, ext(ext))

  #terra::window(r) <- ext3

  r <- setValues(ext, gIds)
  
  out1 <- exact_extract(r,
    geom,
    fun = NULL,
    progress = FALSE,
    include_cols = ID,
    include_cell = TRUE
  )

  out2 <- setnames(rbindlist(out1),
    old = c("cell", "value"),
    new = c("rID", "gID")
  )

  setDT(out2, key = "rID")
  return(out2)
}
