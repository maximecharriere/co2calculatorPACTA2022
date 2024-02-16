
##################################################
# Calculated parameters (don't edit)
##################################################

getParams <- function(Inputs) {
  params <- Inputs

  get_key <- function(year) {
    # Given a year, this function returns the corresponding key.
    # See insulation_historical in constants.json
    # Returns 0 if year is invalid
    key <- 0
    ## Uvalues -> see constants.js
    if (year < 1980) {
      key <- 6
    } else if (year >= 1980 && year <= 1989) {
      key <- 5
    } else if (year >= 1990 && year <= 1999) {
      key <- 4
    } else if (year >= 2000 && year <= 2009) {
      key <- 3
    } else if (year >= 2010 && year <= 2019) {
      key <- 2
    } else if (year > 2019) {
      key <- 1
    }
    return(key)
  }
  insulation_key <- get_key(params$year)
  condition <- sapply(utilisation, function(x) x$key == insulation_key)
  uvalue_index <- which(condition)
  params$uValuesBefore <- constants$insulation_historical[uvalue_index][[1]]


  ### Refurbishments
  ### update u-values with refurbishments
  for (m in params$refurbishments) {
    if (m$measure %in% c("roof", "walls", "floor", "windows")) {
      key <- get_key(m$year)
      if (key < insulation_key) { # only if the renovation is more recent than the construction year
        condition <- sapply(utilisation, function(x) x$key == key)
        uvalue_index <- which(condition)
        temp_uvalues <- constants$insulation_historical[uvalue_index][[1]]
        buildingPart <- m$measure
        params$uValuesBefore[[buildingPart]] <- temp_uvalues[[buildingPart]]
      }
    }
  }

  ## utilisation
  condition <- sapply(utilisation, function(x) x$key == params$utilisation_key)
  utilisation_index <- which(condition)
  params$utilisation <- utilisation[utilisation_index][[1]]

  ## climate
  condition <- sapply(climate, function(x) x$code == params$climate_code)
  climate_index <- which(condition)
  params$climate <- climate[climate_index][[1]]

  ### Elements -> uvalues
  params$opaqueElements <- simpleOpaqueElements(params)
  params$windowElements <- simpleWindowElements(params)

  ### Heat capacity
  params$heatcapacity <- 0.3

  return(params)
}
