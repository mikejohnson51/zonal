.getExtension <- function(file){ 
  ext <- strsplit(basename(file), split="\\.")[[1]]
  return(ext[length(ext)])
} 

#' Execute Spatial Intersection
#' @param file path to a gridded file (either .tif or .nc)
#' @param w a weighting grid produced with `weighting_grid`
#' @return data.table
#' @export
#' @importFrom ncmeta nc_coord_var
#' @importFrom raster raster stack extent xyFromCell cellFromRowCol
#' @importFrom stats na.omit
#' @importFrom RNetCDF open.nc var.get.nc att.get.nc close.nc var.inq.nc
#' @importFrom data.table data.table setkey getDTthreads setDTthreads setnames

execute_zonal    = function(file, w) {
  .SD <-  NULL
  
  ID = names(w)[!names(w) %in% c("grid_id", "w", "X", "Y")]
  
  setkey(w, "grid_id")
  
  if (.getExtension(file) == "nc") {
    nc  = RNetCDF::open.nc(file)
    
    atts     = ncmeta::nc_coord_var(file)
    T_name   = na.omit(unique(atts$T))
    X_name   = na.omit(unique(atts$X))
    Y_name   = na.omit(unique(atts$Y))
    var_name = atts$variable[!atts$variable %in% c(T_name, X_name, Y_name)]
    
    dimid_order <- match(var.inq.nc(nc, var_name)$dimids,
                         c(var.inq.nc(nc, X_name)$dimids,
                           var.inq.nc(nc, Y_name)$dimids,
                           var.inq.nc(nc, T_name)$dimids))
    

    var = RNetCDF::var.get.nc(
      nc,
      var_name,
      start = c(min(w$X), min(w$Y), 1)[dimid_order],
      count = c(max(w$X) - min(w$X) + 1,
                max(w$Y) - min(w$Y) + 1,
                NA)[dimid_order],
      unpack = TRUE
    )
    
 # if(length(dim(var)) == 2){dim(var) = c(dim(var), 1)}  
 # transpose = ifelse(which(dim(var)[1:2] == (max(w$X) - min(w$X) +1)) == 1, TRUE, FALSE)
 # if(transpose)(var = aperm(var, c(2,1,3)))

 time_steps <-
      RNetCDF::utcal.nc(
        RNetCDF::att.get.nc(nc, T_name, "units"),
        RNetCDF::var.get.nc(nc, T_name, unpack = TRUE),
        "c"
      ) %>%
      as.Date() %>%
      as.character()
    
  RNetCDF::close.nc(nc)
    
    if (length(dim(var)) == 2) {
      var = as.numeric(matrix(var, ncol = 1, byrow = FALSE))
    } else {
      dim(var) <- c(dim(var)[dimid_order[1]] * dim(var)[dimid_order[2]], 
                    dim(var)[dimid_order[3]])
    }
    
    dt = data.table::data.table(var)
    
  } else {
    r = suppressWarnings({
      raster(file)
    })
    
    mins = xyFromCell(r, raster::cellFromRowCol(r, min(w$Y), min(w$X)))
    maxs = xyFromCell(r, raster::cellFromRowCol(r, max(w$Y), max(w$X)))
    
    st =  suppressWarnings({
      raster::crop(stack(file),
                   raster::extent(c(mins[1], maxs[1], maxs[2], mins[2])),
                   snap = "out")
    })
    
  
    dt = data.table::data.table(st[])
    time_steps = names(st)
  }
  
  dt$grid_id = 1:dim(dt)[1]
  dt = na.omit(dt)
  setkey(dt, 'grid_id')
  dt = merge(dt, w, by = "grid_id")
  
  cols <- c(grep("V", names(dt), value = TRUE))
  
  threds = getDTthreads()
  setDTthreads(0)
  dt2 = dt[, lapply(.SD, function(x) {
    sum((x * w), na.rm = TRUE)  / sum(w, na.rm = TRUE)
  }),
  keyby = eval(ID), .SDcols = cols]
  setDTthreads(threds)
  
  setnames(dt2, c(ID, time_steps))
  
  dt2
}


