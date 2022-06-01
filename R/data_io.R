# Avoid import from stats, only needed on vector
.na.omit <- function(x) {
  x[!is.na(x)]
}

#' Data Extraction from tif(s) & Raster/spatRaster
#' @param file a in-memory Raster(layer, stack, brick), SpatRaster, or file path(s)
#' @param dims the row and column IDs to extract data between (xmin xmax ymin ymax)
#' @param layer the layer to extract from. If -1 (default), get all layers
#' @return data.table
#' @importFrom terra rast nlyr xFromCol yFromRow align window as.matrix
#' @importFrom data.table setDT

.read_raster <- function(file, dims, layer = -1) {
 
   if (!inherits(file, "SpatRaster")) {
    z <- terra::rast(file)
  } else {
    z <- file
  }

 tmp = xyFromCell(z, dims)
  
  tmp <- ext(c(
    xmin = min(tmp[,1]),
    xmax = max(tmp[,1]),
    ymin = min(tmp[,2]),
    ymax = max(tmp[,2])
  ))

  terra::window(z) <- align(tmp, z, snap = "out")

  df <- as.data.frame(as.matrix(z))

  #names(df) <- paste0("V", 1:ncol(df))

  df$cell <- 1:nrow(df)

  setDT(df, key = "cell")

  if (any(terra::window(z))) {
    terra::window(z) <- NULL
  }

  return(df)
}

#' Data Extraction from NetCDF
#' @param file a netcdf file path
#' @param dims the row and column IDs to extract data between (xmin xmax ymin ymax)
#' @return data.table
#' @importFrom RNetCDF open.nc close.nc var.inq.nc var.get.nc utcal.nc att.get.nc
#' @importFrom ncmeta nc_coord_var
#' @importFrom data.table setDT

.read_nc <- function(file, dims, cell) {
  
  if (inherits(file, "character")) {
    file <- open.nc(file)
  }

  on.exit(close.nc(file), add = TRUE)

  atts <- ncmeta::nc_coord_var(file)

  T_name <- .na.omit(unique(atts$T))
  X_name <- .na.omit(unique(atts$X))
  Y_name <- .na.omit(unique(atts$Y))
  var_name <- atts$variable[!atts$variable %in% c(T_name, X_name, Y_name)]

  if (length(T_name) == 0) {
    dimid_order <- match(
      var.inq.nc(file, var_name)$dimids,
      c(
        var.inq.nc(file, X_name)$dimids,
        var.inq.nc(file, Y_name)$dimids
      )
    )
  } else {
    dimid_order <- match(
      var.inq.nc(file, var_name)$dimids,
      c(
        var.inq.nc(file, X_name)$dimids,
        var.inq.nc(file, Y_name)$dimids,
        var.inq.nc(file, T_name)$dimids
      )
    )
  }

  xmn <- min(dims[1:2])
  xmx <- max(dims[1:2])
  ymn <- min(dims[3:4])
  ymx <- max(dims[3:4])

  var <- RNetCDF::var.get.nc(
    file,
    var_name,
    start = c(xmn, ymn, 1)[dimid_order],
    count = c(
      (xmx - xmn) + 1,
      (ymx - ymn) + 1,
      NA
    )[dimid_order],
    unpack = TRUE
  )

  if (length(dim(var)) == 2) {
    var <- as.numeric(matrix(var, ncol = 1, byrow = FALSE))
  } else {
    dim(var) <- c(
      dim(var)[dimid_order[1]] * dim(var)[dimid_order[2]],
      dim(var)[dimid_order[3]]
    )
  }

  df <- as.data.frame(var)
  names(df) <- paste0("V", 1:ncol(df))
  df$cell <- cell
  setDT(df, key = "cell")
}


#' Data Extraction
#' @param file a netcdf file path, set if tif file paths, or
#' in-memory Raster(layer, stack, brick), SpatRaster, or file path(s)
#' @param w a weight grid object
#' @return data.table
#' @export
#' @importFrom data.table merge.data.table

.zonal_io <- function(file, w) {
  
  r = rast(file)
  ind = "cell"

  if(!"cell" %in% names(w)){
    if("rID" %in% names(w)){
     w = dplyr::rename(w, cell = rID)
  } else {
    stop('need "cell" or "rID" column in weight grid (w)', call. = FALSE)
  }}
  
  tmp = data.frame(terra::xyFromCell(r, w[[ind]]))

  ext = terra::ext(c(min(tmp[,'x']),
                     max(tmp[,'x']),
                     min(tmp[,'y']),
                     max(tmp[,'y']))) |> 
    terra::align(r, snap = "out")
  
  cells = terra::cells(r, ext)
  
  tmp = terra::rowColFromCell(r, cells)
  
  dims = c(
    xmin = min(tmp[,2]),
    xmax = max(tmp[,2]),
    ymin = min(tmp[,1]),
    ymax = max(tmp[,1])
  )
  
  if (inherits(file, "SpatRaster") | inherits(file, "Raster")) {
    out <- .read_raster(file, dims = w[[ind]])
  } else if (grepl("netcdf", class(file), ignore.case = TRUE)) {
    out <- .read_nc(file, dims, cell = w[[ind]])
  } else if (class(file) == "character") {
    if (.getExtension(file) == "nc") {
      out <- .read_nc(file, dims, cell = cells)
    } else {
      out <- .read_raster(file, w[[ind]])
    }
  } else {
    stop("error in reading of input...")
  }

  merge.data.table(out, w, by = eval(ind))
}
