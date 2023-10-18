AOI <- AOI::aoi_get(state = "south", county = "all")
d = rast("to_build/pr_2022.nc")


test_that("errors", {
  
  expect_error(execute_zonal(data = NULL, geom = AOI, id = "fip_code"))
  expect_error(execute_zonal(data = d, geom = NULL, id = "fip_code"))
  expect_error(execute_zonal(data = d, geom = AOI, w = data.frame(), id = "fip_code"))
  
})


test_that("read", {
  v = 33.126068
  df <- execute_zonal(data = d[[1]], 
                      geom = AOI[1,], 
                      ID = "fip_code", 
                      join = FALSE)
  
  expect_true(inherits(df, "data.frame"))
  
  expect_equal(df$fip_code, "01001")
  expect_equal(df$`mean,precipitation_amount_day=44560`, v)
  
  
  df2 <- execute_zonal(data = d[[1:2]], 
                      geom = AOI[1,], 
                      ID = "fip_code", 
                      join = FALSE)
  
  expect_true(inherits(df, "data.frame"))
  
  expect_equal(df2$fip_code, "01001")
  expect_equal(df2$`mean,precipitation_amount_day=44560`, v)
  expect_equal(ncol(df2), 3)
  
  df3 <- execute_zonal(data = d[[1:2]], 
                       geom = AOI[1:2,], 
                       ID = "fip_code", 
                       join = FALSE)
  
  expect_true(inherits(df, "data.frame"))
  
  expect_equal(df3$fip_code[1], "01001")
  expect_equal(df3$`mean,precipitation_amount_day=44560`[1], v)
  expect_equal(nrow(df3), 2)
  expect_equal(ncol(df3), 3)
  
  spa <- execute_zonal(data = d[[1]], 
                       geom = AOI[1,], 
                       ID = "fip_code", 
                       join = TRUE)
  
  expect_true(inherits(spa, "sf"))
  
  expect_equal(spa$fip_code, "01001")
  expect_equal(spa$`mean,precipitation_amount_day=44560`, v)
  
  df <- execute_zonal(data = d[[1]], 
                      geom = AOI[1,], 
                      ID = "fip_code", 
                      fun = circular_mean,
                      join = FALSE)
  
  df2 <- execute_zonal(data = d[[1]], 
                      geom = AOI, 
                      ID = "fip_code", 
                      fun = circular_mean,
                      join = FALSE)
  
  df3 <- execute_zonal(data = d[[1:6]], 
                       geom = AOI[1,], 
                       ID = "fip_code", 
                       fun = circular_mean,
                       join = FALSE)
  
  expect_true({
    df$`fun,precipitation_amount_day=44560` == df2$`fun,precipitation_amount_day=44560`[1]
  })
  
  expect_true({
    df$`fun,precipitation_amount_day=44560` == df3$`precipitation_amount_day=44560`
  })

  
})


a = AOI::aoi_get(state = "CO", county = "Larimer")
r = climateR::getGridMET(AOI = a,  varname = "srad", startDate = "2020-10-29")

execute_zonal(data = r, 
              geom = a, 
              ID = "fip_code", 
              fun = "mean",
              join = FALSE)

