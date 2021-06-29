#' Execute Spatial Intersection on Categorical Data
#' @param file path to a gridded file (either .tif or .nc)
#' @param w a weighting grid produced with `weighting_grid`
#' @param rcl a data.frame with columns names `from` and `to`. 
#' `From` should be the categorical values used in the raster, 
#' while `to` should be the categorical names to use in the output table headings.
#' If left NULL (default) the headings will not be altered.
#' @return a data.table
#' @export
#' @importFrom raster stack ncell xyFromCell cellFromRowCol crop extent
#' @importFrom data.table data.table setnames setkey

execute_zonal_cat    = function(file, w,  rcl = NULL){
  
  ID = names(w)[!names(w) %in% c("grid_id", "w", "X", "Y")]
  
  r = suppressWarnings(raster(file))
  
  mins = xyFromCell(r[[1]], raster::cellFromRowCol(r, min(w$Y), min(w$X)))
  maxs = xyFromCell(r[[1]], raster::cellFromRowCol(r, max(w$Y), max(w$X)))
  
  st =  suppressWarnings({
    raster::crop(r,
                 raster::extent(c(mins[1], maxs[1], maxs[2], mins[2])),
                 snap = "out")
  })
  
  dt = data.table::data.table(cbind(grid_id = 1:raster::ncell(st) , value = st[]), key = "grid_id")
  dt = merge(dt, w[,.(grid_id, w, ID = get(ID))], by = "grid_id")
  dt[, frac_total:= w / sum(w, na.rm = TRUE), by = c('ID')]
  dt2 = dt[, .(freq = sum(frac_total, na.rm = TRUE)), by = c('ID', 'value')]
  
  if(!is.null(rcl)){
    dt2$value = rcl$to[match(dt2$value, rcl$from)]
  }
  
  setnames(dt2, c(ID, "value", "percentage"))
}


# file = '/Users/mjohnson/Downloads/MCD12Q1.006.nc'
# 
# rcl = read.csv('inst/modis_lc.csv') %>% 
#   dplyr::select(from = Class, to = Class.name )

# execute_zonal_cat    = function(file, w,  rcl = NULL){
#   
#   . <- area_m2 <- area_tot <- tot <-  NULL
#   
#   ID = names(w)[!names(w) %in% c("grid_id", "w", "X", "Y")]
#   
#   r = suppressWarnings(stack(file))
#   if(nlayers(r) > 1){warning("Only 1 layer can be processed at a time. Working with layer 1.")}
#   
#   cellArea_m2 = sf::st_bbox(r) %>% 
#     sf::st_as_sfc() %>% 
#     sf::st_transform(5070) %>% 
#     sf::st_area() / raster::ncell(r)
#   
#   w$area_m2 = as.numeric(w$w * cellArea_m2)
#   
#   w2 = w[ ,.(area_tot = sum(area_m2)), by = ID]
#   
#   
#   mins = xyFromCell(r, raster::cellFromRowCol(r, min(w$Y), min(w$X)))
#   maxs = xyFromCell(r, raster::cellFromRowCol(r, max(w$Y), max(w$X)))
#   st =  suppressWarnings({
#     raster::crop(r[[1]],
#                  raster::extent(c(mins[1], maxs[1], maxs[2], mins[2])),
#                  snap = "out")
#   })
#   
#   dt = data.table::data.table(st[])
#   
#   dt$grid_id = 1:dim(dt)[1]
#   dt = na.omit(dt)
#   setkey(dt, 'grid_id')
#   dt = merge(dt,w, by = "grid_id")
#   
#   cols <- c(grep("V", names(dt), value = TRUE))
#   
#   threds = getDTthreads()
#   setDTthreads(0)
# 
#   dt2 = dt[, .(tot = sum(area_m2)) , by = c(ID, "V1")]
#   dt3 = merge(dt2,w2)
#   dt3 = dt3[,tot := round(tot / area_tot, 2)]
#   piv = dcast(dt3, get(ID) ~ V1, value.var = "tot", fill = 0)
#   
#   setDTthreads(threds)
#   
#   if(!is.null(rcl)){
#     m = suppressWarnings({ match(as.numeric(names(piv)), rcl$from) }) 
#     new_names = ifelse(is.na(m), names(piv), gsub(" ", "_",rcl$to[m]))
#     setnames(piv, new_names)
#   }
# 
#   piv
# }
