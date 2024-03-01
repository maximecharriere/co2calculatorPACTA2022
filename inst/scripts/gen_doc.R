# First install pandoc https://github.com/jgm/pandoc/releases
devtools::document() # Equivalent to roxygenize(), also updates documents
devtools::build() # Build the package
devtools::install() # Install the package to your R library
pkgdown::build_site()
