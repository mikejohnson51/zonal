# Avoid import from stats, only needed on vector
.na.omit = function(x){ x[!is.na(x)] }

#' Data Extration from tif(s) & Raster/spatRaster
#' @param file a in-memory Raster(layer, stack, brick), SpatRaster, or file path(s)
#' @param w a weight grid
#' @param layer the layer to extract from. If -1 (default), get all layers
#' @return data.table
#' @importFrom terra rast xyFromCell cellFromRowCol crop ext values sources nlyr
#' @importFrom data.table setDT

.read_raster = function(file, w, layer = -1){
  if(class(file) != "SpatRaster"){ file = terra::rast(file) }
  
  if(max(layer) < terra::nlyr(file) & min(layer) > 0 ){
    file = file[[layer]]
  }

  mins = terra::xyFromCell(file, terra::cellFromRowCol(file, min(w$Y), min(w$X)))
  maxs = terra::xyFromCell(file, terra::cellFromRowCol(file, max(w$Y), max(w$X)))

  cats =  terra::crop(file, 
                      terra::ext(c(mins[1], maxs[1], maxs[2], mins[2])),
                      snap = "out")
  
  df = as.data.frame(cats[])

  names(df) <- paste0('V', 1:ncol(df))
  df$grid_id = 1:nrow(df)
  setDT(df, key = "grid_id")
}



#' Data Extration from NetCDF
#' @param file a netcdf file path
#' @param w a weight grid
#' @return data.table
#' @importFrom RNetCDF open.nc close.nc var.inq.nc var.get.nc utcal.nc att.get.nc
#' @importFrom ncmeta nc_coord_var
#' @importFrom data.table setDT

.read_nc = function(file, w){
  
  if(class(file) == "character"){ file = open.nc(file) }
  on.exit(close.nc(file), add  = TRUE)
  atts     = ncmeta::nc_coord_var(file)
  T_name   = .na.omit(unique(atts$T))
  X_name   = .na.omit(unique(atts$X))
  Y_name   = .na.omit(unique(atts$Y))
  var_name = atts$variable[!atts$variable %in% c(T_name, X_name, Y_name)]
  
  if(length(T_name) == 0){
    dimid_order = match(var.inq.nc(file, var_name)$dimids,
                        c(var.inq.nc(file, X_name)$dimids,
                          var.inq.nc(file, Y_name)$dimids))
  } else {
    dimid_order = match(var.inq.nc(file, var_name)$dimids,
                        c(var.inq.nc(file, X_name)$dimids,
                          var.inq.nc(file, Y_name)$dimids,
                          var.inq.nc(file, T_name)$dimids))
  } 
  
  
  var = RNetCDF::var.get.nc(
    file,
    var_name,
    start = c(min(w$X), min(w$Y), 1)[dimid_order],
    count = c(max(w$X) - min(w$X) + 1,
              max(w$Y) - min(w$Y) + 1,
              NA)[dimid_order],
    unpack = TRUE
  )
  
  time_steps <-
    RNetCDF::utcal.nc(
      RNetCDF::att.get.nc(file, T_name, "units"),
      RNetCDF::var.get.nc(file, T_name, unpack = TRUE),
      "c"
    ) %>%
    as.Date() %>%
    as.character()

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
#' @param w a weight grid
#' @param layer the layer to extract from. If -1 (default), get all layers
#' @return data.table
#' @export
#' @importFrom data.table merge.data.table

.zonal_io = function(file, w, layer) {
  if (grepl("raster", class(file), ignore.case = TRUE)) {
    out = .read_raster(file, w)
  } else if (grepl("netcdf", class(file), ignore.case = TRUE)) {
    out = .read_nc(file, w)
  } else if (class(file) == "character") {
    if (.getExtension(file) == 'nc') {
      out = .read_nc(file, w)
    } else {
      out = .read_raster(file, w)
    }
  } else {
    stop("error in reading of input...")
  }
  
  merge.data.table(out, w, by = "grid_id")
} 


