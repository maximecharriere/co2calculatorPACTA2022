# First install pandoc https://github.com/jgm/pandoc/releases
library(roxygen2)
library(devtools)
library(pkgdown)
library(usethis)
devtools::document() # Equivalent to roxygenize(), also updates documents
devtools::install() # Install the package to your R library
pkgdown::build_site()
# use_pkgdown_github_pages()