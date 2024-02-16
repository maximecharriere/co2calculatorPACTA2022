# Display message on package loading
.onAttach <- function(libname, pkgname) {
  msg <- sprintf("Package %s %s loaded\nNeed help? --> `vignette(\"co2calculatorPACTA2022\")`", pkgname, utils::packageVersion(pkgname))
  packageStartupMessage(msg)
}
