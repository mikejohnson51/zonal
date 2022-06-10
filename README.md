
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zonal <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R CMD
Check](https://github.com/mikejohnson51/zonal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/zonal/actions/workflows/R-CMD-check.yaml)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![LifeCycle](man/figures/lifecycle/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Dependencies](https://img.shields.io/badge/dependencies-6/31-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![Website
deployment](https://github.com/mikejohnson51/zonal/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mikejohnson51/zonal/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

`zonal` is an active package for intersecting vector aggregation units
with large gridded data. While there are many libraries that seek to
tackle this problem (see credits) we needed a library that could handle
large gridded extents storing categorical and continuous data, with
multiple time layers with both many small vector units and few large
units.

The package offers 3 main options through a common syntax:

1.  The ability to pregenerate weighting grids and applying those over
    large datasets distrbuted across files (e.g. 30 years of daily data
    stored in annula files). Rapid data summarization is supported by
    collapse and data.table.

2.  Thin wrappers over pure exact_extract() when appropriate
    (e.g. calling a core `exactrextractr` function)

3.  Flexible custom functions that are easily applied over multi layer
    files (e.g. geometric means and circular means)

## Installation

You can install the development version of `zonal` with:

``` r
# install.packages("remotes")
remotes::install_github("mikejohnson51/zonal")
```

## Example

This is a basic example that takes a NetCDF file containing a 4km grid
for the continental USA and daily precipitation for the year 1979 (365
layers). Our goal is to subset this file to the southern USA, and
compute daily county level averages. The result is a daily rainfall
average for each county.

``` r
library(zonal)

AOI <- AOI::aoi_get(state = "south", county = "all")

system.time({
  pr_zone <- execute_zonal(data = "to_build/pr_2020.nc", 
                           geom = AOI, 
                           ID = "fip_code", 
                           join = FALSE)
})
#>    user  system elapsed 
#>   5.256   0.599   5.890

# PET zone: Counties, time slices/ID
dim(pr_zone)
#> [1] 1421  367
```

### Daily maximum mean rainfall in a county?

``` r
x <- merge(AOI, pr_zone, by = "fip_code")
# Plot Day with the maximum single county max rainfall.
n <- colnames(pr_zone)[which(pr_zone[, -1] == max(pr_zone[, -1]), arr.ind = TRUE)[2] + 1]

ggplot(data = x) +
  geom_sf(aes(fill = get(n)), color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "PR (mm)")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Daily maximum rainfall in the south?

``` r
# Plot Day with the maximum county wide rainfall
n2 <- names(which.max(colSums(select(pr_zone, -fip_code))))

ggplot() +
  geom_sf(data = x, aes(fill = get(n2)), color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "PR (mm)")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Timeseries of county with maximum annual rainfall

``` r
data <- pr_zone %>%
  slice_max(rowSums(select(., -fip_code))) %>%
  pivot_longer(-fip_code, names_to = "day", values_to = "prcp") %>%
  mutate(day = as.numeric(gsub("mean.precipitation_amount_day=", "", day)))

head(data)
#> # A tibble: 6 × 3
#>   fip_code   day   prcp
#>   <chr>    <dbl>  <dbl>
#> 1 37175    43829  0    
#> 2 37175    43830 26.8  
#> 3 37175    43831 13.7  
#> 4 37175    43832  0.244
#> 5 37175    43833  0.144
#> 6 37175    43834  2.55
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

# 1km Landcover Grid (Categorical)

One of the largest limitations of existing utilities is the ability to
handle categorical data. Here we show an example for a 1km grid storing
land cover data from MODIS. This grid was creating by mosacing 19 MODIS
tiles covering CONUS. The summary function for this categorical
frequency is “freq”.

``` r
system.time({
  lc <- execute_zonal(data = "to_build/2019-01-01.tif", 
                      geom = AOI, 
                      ID = "fip_code", 
                      fun = "frac")
})
#>    user  system elapsed 
#>   2.218   0.105   2.341
```

## Zonal and opendap.catalog

Here lets look at a quick intergation of the
`AOI`/`opendap.catalog`/`zonal` family. The goal is to find monthly
mean, normal (1981-2010), rainfall for all USA counties in the south.

``` r
library(opendap.catalog)

AOI <- AOI::aoi_get(state = "FL", county = "all")

system.time({
  file <- opendap.catalog::dap(
    URL = "https://cida.usgs.gov/thredds/dodsC/bcsd_obs",
    AOI = AOI,
    startDate = "1995-01-01",
    verbose = FALSE,
    varname  = "pr"
  ) |>
    execute_zonal(geom = AOI, fun = "mean", ID = "fip_code", join = TRUE)
})
#>    user  system elapsed 
#>   0.935   0.029   1.838

plot(file["mean"], border = NA)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

------------------------------------------------------------------------

## Getting involved

1.  Code style should attempt to follow the tidyverse style guide.
2.  Please avoid adding significant new dependencies without a
    documented reason why.
3.  Please attempt to describe what you want to do prior to contributing
    by submitting an issue.
4.  Please follow the typical github fork - pull-request workflow.
5.  Make sure you use roxygen and run Check before contributing.

------------------------------------------------------------------------

## Open source licensing info

1.  [TERMS](TERMS.md)
2.  [LICENSE](LICENSE)

------------------------------------------------------------------------

## Credits and references

Similar R packages:

1.  [exactexactr](https://github.com/isciences/exactextractr)
2.  [sf](https://github.com/r-spatial/sf)
3.  [terra](https://github.com/rspatial/raster)

**Logo Artwork:** [Justin
Singh-Mohudpur](https://www.justinsingh.me/about/)
