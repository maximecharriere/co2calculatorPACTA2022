#' pkg_install
#'
#' Install one or more package from a given CRAN repository,
#' is they're not already installed.
#'
#' @param packages Vector of packages to install.
#' @param repos Link to the CRAN repo to get the packages.
#'
#' @examples
#' pkg_install(packages = c("knitr", "readxl"), repos = "https://stat.ethz.ch/CRAN")
pkg_install <- function(packages, repos = "https://stat.ethz.ch/CRAN") {
  packagecheck <- match(packages, utils::installed.packages()[, 1])
  packagestoinstall <- packages[is.na(packagecheck)]
  if (length(packagestoinstall) > 0L) {
    utils::install.packages(packagestoinstall,
      repos = repos
    )
  } else {
    print("All requested packages already installed")
  }
}

pkg_install(packages = c(
  "tidyverse" #
  , "languageserver" # Language server for R
  , "writexl" # Write Excel file
  , "readxl" # Read Excel file
  , "jsonlite" # Read JSON file
  , "roxygen2" # Generate documentation
  , "pkgdown" # Generate package website
  , "devtools" # Package development tools
  , "usethis" # Automate package setup
), repos = "https://stat.ethz.ch/CRAN")
