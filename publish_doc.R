# First install pandoc https://github.com/jgm/pandoc/releases
library(roxygen2)
library(devtools)
library(pkgdown)
devtools::document() # Equivalent to roxygenize(), also updates documents
devtools::install() # Install the package to your R library
pkgdown::build_site()
