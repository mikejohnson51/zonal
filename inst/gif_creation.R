
file = list.files('/Volumes/Transcend/ngen/MODIS/MOD13A3.006/conus', full.names = TRUE)
geom  = AOI::aoi_get(state = "south", county = "all")

w      = weighting_grid(file, geom, "geoid")

gifski::save_gif({
  for(t in 1:240) {
    try({
      plot(oo[ids[t]], border = F, pal = pal, breaks = b, main = paste("LAI:", ids[t]))
    }, silent = F)
  }}, gif_file = "img/lai.gif", width = 400, height = 300, delay = .15, loop = TRUE)


file = '/Users/mjohnson/Downloads/pr_1979.nc'
geom  = AOI::aoi_get(state = "south", county = "all")
w = weighting_grid(file, geom, "geoid")

system.time({
  w      = weighting_grid(file, geom, "geoid")
  zz     = execute_zonal(file, w)
})

oo = merge(geom, zz)
ids = grep("V", names(oo), value = TRUE)
b = seq(min(st_drop_geometry(oo[,ids])), max(st_drop_geometry(oo[,ids])), length.out = 10)
pal = (blues9)


gifski::save_gif({
  for(t in 1:365) {
    try({
      plot(oo[ids[t]], border = "black", pal = pal, 
           breaks = b, main = paste("PR:", ids[t]), lwd = .5)
    }, silent = F)
  }}, gif_file = "img/pr.gif", width = 800, height = 600, delay = .1, loop = TRUE)

