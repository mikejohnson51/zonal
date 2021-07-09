library(knitr)
fs::file_copy("inst/01_weight_creation.Rmd", 'vignettes/01_weight_creation.Rmd.orig', overwrite = TRUE)
knit("vignettes/01_weight_creation.Rmd.orig", "vignettes/01_weight_creation.Rmd")

fs::file_copy("inst/02_intersections.Rmd", 'vignettes/02_intersections.Rmd.orig', overwrite = TRUE)
knit("vignettes/02_intersections.Rmd.orig", "vignettes/02_intersections.Rmd")

fs::file_copy("inst/03_categorical.Rmd", 'vignettes/03_categorical.Rmd.orig', overwrite = TRUE)
knit("vignettes/03_categorical.Rmd.orig", "vignettes/03_categorical.Rmd")

files = list.files(".", ".png", full.names = TRUE)
fs::file_move(files, paste0('vignettes/', basename(files)))

pkgdown::build_site()
