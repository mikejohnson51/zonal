# Avoid import from stats, only needed on vector
.na.omit = function(x){ x[!is.na(x)] }

#' Data Extraction from tif(s) & Raster/spatRaster
#' @param file a in-memory Raster(layer, stack, brick), SpatRaster, or file path(s)
#' @param dims the row and column IDs to extract data between (xmin xmax ymin ymax)
#' @param layer the layer to extract from. If -1 (default), get all layers
#' @return data.table
#' @importFrom terra rast nlyr xFromCol yFromRow align window as.data.frame 
#' @importFrom data.table setDT

.read_raster = function(file, dims, layer = -1){
  
  if(class(file) != "SpatRaster"){
    z = terra::rast(file) 
  } else {
    z = file
  }
  
  tmp = ext(c(
    xFromCol(z, dims[1]),
    xFromCol(z, dims[2]),
    yFromRow(z, dims[3]),
    yFromRow(z, dims[4])
  ))
  
  terra::window(z) <- terra::align(tmp, z, snap = "out")

  df = terra::as.data.frame(z, na.rm = FALSE)

  names(df) <- paste0('V', 1:ncol(df))
  df$grid_id = 1:nrow(df)
  
  setDT(df, key = "grid_id")
  
  
  if(terra::window(z)){
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

.read_nc = function(file, dims){
  
  if(class(file) == "character"){ file = open.nc(file) }
  
  on.exit(close.nc(file), add  = TRUE)
  
  atts     = ncmeta::nc_coord_var(file)
  
  T_name   = .na.omit(unique(atts$T))
  X_name   = .na.omit(unique(atts$X))
  Y_name   = .na.omit(unique(atts$Y))
  var_name = atts$variable[!atts$variable %in% c(T_name, X_name, Y_name)]
  
  if(length(T_name) == 0){
    dimid_order = match(var.inq.nc(file,   var_name)$dimids,
                        c(var.inq.nc(file, X_name)$dimids,
                          var.inq.nc(file, Y_name)$dimids))
  } else {
    dimid_order = match(var.inq.nc(file,  var_name)$dimids,
                        c(var.inq.nc(file, X_name)$dimids,
                          var.inq.nc(file, Y_name)$dimids,
                          var.inq.nc(file, T_name)$dimids))
  } 
  
  xmn = min(dims[1:2])
  xmx = max(dims[1:2])
  ymn = min(dims[3:4])
  ymx = max(dims[3:4])
  
  var = RNetCDF::var.get.nc(
    file,
    var_name,
    start = c(xmn, ymn, 1)[dimid_order],
    count = c((xmx - xmn) + 1,
              (ymx - ymn) + 1,
              NA)[dimid_order],
    unpack = TRUE
  )

  if (length(dim(var)) == 2) {
    var = as.numeric(matrix(var, ncol = 1, byrow = FALSE))
  } else {
    dim(var) <- c(dim(var)[dimid_order[1]] * dim(var)[dimid_order[2]], 
                  dim(var)[dimid_order[3]])
  }
  
  df = as.data.frame(var)
  names(df) <- paste0('V', 1:ncol(df))
  df$grid_id = 1:nrow(df)
  setDT(df, key = "grid_id")
}


#' Data Extraction
#' @param file a netcdf file path, set if tif file paths, or 
#' in-memory Raster(layer, stack, brick), SpatRaster, or file path(s)
#' @param w a weight grid object
#' @return data.table
#' @export
#' @importFrom data.table merge.data.table

.zonal_io = function(file, w) {
  
  dims = w$dims
  
  if (grepl("raster", class(file), ignore.case = TRUE)) {
    out = .read_raster(file, dims)
  } else if (grepl("netcdf", class(file), ignore.case = TRUE)) {
    out = .read_nc(file, dims)
  } else if (class(file) == "character") {
    if (.getExtension(file) == 'nc') {
      out = .read_nc(file, dims)
    } else {
      out = .read_raster(file, dims)
    }
  } else {
    stop("error in reading of input...")
  }
  
  merge.data.table(out, w$weight_map, by = "grid_id")
}

