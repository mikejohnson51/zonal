
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zonal

<!-- badges: start -->

[![R CMD
Check](https://github.com/mikejohnson51/zonal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/zonal/actions/workflows/R-CMD-check.yaml)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

`zonal` is an active package for intersecting vector aggregation units
with large gridded data. While there are many libraries that seek to
tackle this problem (see credits) we needed a library that could handle
large gridded extents with multiple time layers with both many small
vector units and few large units.

We also seek to segment the creation of grid weights from the zonal
execution so that the same weight map can be applied across different
products with the same structure (e.g. MODIS ET and MODIS LAI)

## Installation

You can install the development version of `zonal` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mikejohnson51/zonal")
```

## Example

This is a basic example:

``` r
library(zonal)

file = '/Users/mjohnson/Downloads/pr_1979.nc'
AOI = AOI::aoi_get(state = "south", county = "all") %>% 
  sf::st_transform(5070)

system.time({
  # Build Weight Grid
  w        = weighting_grid(file, AOI, "geoid")
  # Intersect
  pr_zone = execute_zonal(file, w)
})
#>    user  system elapsed 
#>  12.499   2.287  10.692

# PET zone: Counties, time slices/ID
dim(pr_zone)
#> [1] 1421  366
```

``` r
# Plot Day with the maximum single county max rainfall.
n = colnames(pr_zone)[which(pr_zone[,-1] == max(pr_zone[,-1]), arr.ind = TRUE)[2] + 1]
plot(merge(AOI, pr_zone)[n], border = NA)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
# Plot Day with the maximum county wide rainfall
n2 = names(which.max(colSums(select(pr_zone, -geoid))))
plot(merge(AOI, pr_zone)[n2], border = NA)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
library(ggplot2)
data = pr_zone %>% 
  slice_max(rowSums(select(., -geoid))) %>% 
  tidyr::pivot_longer(-geoid, names_to = "date", values_to = "prcp") 

ggplot(data) +
  aes(x = as.Date(date), y = prcp) + 
  geom_line() + 
  labs(x = "Date", y = "Mean Rainfall",
       title = paste("GEOID: ", data$geoid[1])) +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

# 1km Landcover Grid (Categorical)

``` r
file = '/Users/mjohnson/Downloads/MCD12Q1.006.nc'
rcl = read.csv("inst/modis_lc.csv") %>% 
  select(from = Class, to = short)

system.time({
  # Build Weight Grid
  w  = weighting_grid(file, AOI, "geoid")
  # Intersect, and relclassify
  lc = execute_zonal_cat(file, w, rcl)
})
#>    user  system elapsed 
#>   7.310   0.874   5.100
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

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
2.  [intersectr](https://github.com/USGS-R/intersectr)
3.  [areal](https://github.com/slu-openGIS/areal)
4.  [sf](https://github.com/r-spatial/sf)
5.  [raster](https://github.com/rspatial/raster)
