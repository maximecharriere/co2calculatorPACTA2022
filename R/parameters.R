##################################################
# Calculated parameters 
##################################################

#' Generate Simple Opaque Building Elements
#'
#' `simpleOpaqueElements` generates a list of simple opaque building elements based on the provided
#' parameters. This includes the roof, floor, and walls of the building, with each element's thermal
#' properties (U-values) and areas calculated according to the input parameters and predefined constants
#' within the function. This function is useful for estimating the thermal characteristics of a building's
#' envelope in a simplified manner.
#'
#' @param params A list containing parameters for the building, including:
#'   \describe{
#'     \item{uValuesBefore}{A list of U-values for various building elements.}
#'     \item{area}{Total floor area of the building.}
#'     \item{floors}{Number of floors in the building.}
#'   }
#'
#' @return A list of lists, where each sublist represents an opaque building element (roof, floor, or walls).
#'   Each element includes:
#'   \describe{
#'     \item{name}{The name of the building element.}
#'     \item{uValue}{The thermal transmittance (U-value) of the element.}
#'     \item{area}{The area of the element in square meters.}
#'     \item{factor}{A factor used in calculations, set to 1 for all elements in this simplified model.}
#'   }
#'
#' @details The function simplifies the calculation by assuming standard building geometries and
#'   applying fixed ratios for window areas and roof angles. It is intended for preliminary
#'   assessments and may not substitute detailed thermal modeling.
#'
#' @export
simpleOpaqueElements <- function(params) {
  uvalues <- params$uValuesBefore
  areaPerFloor <- params$area / params$floors * constants$buildingGenerator$areaEbfRatio
  buildingWidth <- min(sqrt(areaPerFloor), constants$buildingGenerator$maxBuildingWidth)
  buildingLength <- areaPerFloor / buildingWidth
  buildingHeight <- constants$buildingGenerator$floorHeight * params$floors

  roofArea <- buildingWidth * buildingLength / cos(constants$buildingGenerator$roofAngle * pi / 180)
  roofAreaOpaque <- roofArea * (1 - constants$buildingGenerator$roofWindowRatio)
  roof <- list()
  roof$name <- "Dach"
  roof$uValue <- uvalues$roof
  roof$area <- roofAreaOpaque
  roof$factor <- 1

  floorArea <- buildingWidth * buildingLength
  floor <- list()
  floor$name <- "Kellerdecke"
  floor$uValue <- uvalues$floor
  floor$area <- floorArea
  floor$factor <- 1

  wallsArea <- 2 * buildingWidth * buildingHeight + 2 * buildingLength * buildingHeight
  wallsAreaOpaque <- wallsArea * (1 - constants$buildingGenerator$windowRatio)
  walls <- list()
  walls$name <- "Fassade"
  walls$uValue <- uvalues$walls
  walls$area <- wallsAreaOpaque
  walls$factor <- 1

  return(list(roof, floor, walls))
}

#' Generate Simple Window Elements for a Building
#'
#' `simpleWindowElements` calculates and generates a list of window elements for a building based on provided
#' parameters and predefined constants. This includes roof windows and side windows with specific characteristics
#' such as orientation, thermal transmittance (U-values), area, and factors affecting light and heat transmission
#' through the windows. This function is useful for estimating the thermal and lighting characteristics of a
#' building's window elements in a simplified manner.
#'
#' @param params A list containing parameters for the building, including:
#'   \describe{
#'     \item{uValuesBefore}{A list of U-values for various building elements before any refurbishment.}
#'     \item{area}{Total floor area of the building.}
#'     \item{floors}{Number of floors in the building.}
#'   }
#'
#' @return A list of window elements where each element is a list containing:
#'   \describe{
#'     \item{name}{Name of the window element, indicating its type and orientation.}
#'     \item{orientation}{Orientation of the window ('H' for roof windows, 'S', 'W', 'N', 'E' for side windows).}
#'     \item{uValue}{The thermal transmittance (U-value) of the window.}
#'     \item{area}{The area of the window in square meters.}
#'     \item{opacity}{Light opacity of the window. Constant at 0.6 for all windows in this simplified model.}
#'     \item{frameFactor}{The frame factor, representing the proportion of the window area that is framing. Constant at 0.7 for all windows in this simplified model.}
#'     \item{shadingFactor}{A factor representing the effect of shading devices on the window. Constant at 0.8 for all windows in this simplified model.}
#'   }
#'
#' @details The function assumes standard building geometries, applies fixed ratios for window-to-wall area
#' and roof angles and standard factors for windows properties. It is designed for preliminary assessments and may not substitute detailed thermal or
#' lighting modeling.
#'
#' @export
simpleWindowElements <- function(params) {
  uvalues <- params$uValuesBefore
  areaPerFloor <- params$area / params$floors * constants$buildingGenerator$areaEbfRatio
  buildingWidth <- min(sqrt(areaPerFloor), constants$buildingGenerator$maxBuildingWidth)
  buildingLength <- areaPerFloor / buildingWidth
  buildingHeight <- constants$buildingGenerator$floorHeight * params$floors

  roofArea <- buildingWidth * buildingLength / cos(constants$buildingGenerator$roofAngle * pi / 180)
  roofWindowArea <- roofArea * constants$buildingGenerator$roofWindowRatio
  roofwindow <- list()
  roofwindow$name <- "Dachfenster"
  roofwindow$orientation <- "H"
  roofwindow$uValue <- uvalues$windows
  roofwindow$area <- roofWindowArea
  roofwindow$opacity <- 0.6
  roofwindow$frameFactor <- 0.7
  roofwindow$shadingFactor <- 0.8

  results <- c()
  results[[1]] <- roofwindow
  i <- 2

  wallsArea <- 2 * buildingWidth * buildingHeight + 2 * buildingLength * buildingHeight
  windowArea <- wallsArea * constants$buildingGenerator$windowRatio
  for (orientation in c("S", "W", "N", "E")) {
    result <- list()
    result$name <- paste("Fenster ", orientation)
    result$orientation <- orientation
    result$uValue <- uvalues$windows
    result$area <- windowArea / 4
    result$opacity <- 0.6
    result$frameFactor <- 0.7
    result$shadingFactor <- 0.8
    results[[i]] <- result
    i <- i + 1
  }
  return(results)
}

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

#' Gather and Validate Parameters for CO2 Emission Calculations
#'
#' This function validates the input parameters and prepare the \code{input_args} list for further processing .
#' by the \code{\link{getParams}} function. It ensures that all mandatory parameters are present and
#' correctly formatted, and it collects information about any refurbishments that have been carried out.
#'
#' @param params See \code{\link{calculate_emissions}} for details on the parameters.
#' @return A list containing all validated and formatted input parameters, including any
#'   refurbishment details.
#'
#' @export
get_parameters <- function(area, floors, year, utilisation_key, climate_code, energy_carrier, walls_refurb_year = NULL, roof_refurb_year = NULL, windows_refurb_year = NULL, floor_refurb_year = NULL, heating_install_year = NULL) {
  input_args <- list()
  input_args_refurbishments <- list()

  # Validate input arguments
  if (missing(area)) {
    stop("Mandatory parameter 'area' is missing")
  } else if (is.null(area)) {
    stop("Mandatory parameter 'area' is NULL")
  } else if (is.na(area)) {
    stop("Mandatory parameter 'area' is NA")
  } else if (is.numeric(area) & area > 0) {
    input_args$area <- area
  } else {
    stop(paste("Invalid parameter 'area' -->", area))
  }

  if (missing(floors)) {
    stop("Mandatory parameter 'floors' is missing")
  } else if (is.null(floors)) {
    stop("Mandatory parameter 'floors' is NULL")
  } else if (is.na(floors)) {
    stop("Mandatory parameter 'floors' is NA")
  } else if (is.numeric(floors) & floors > 0) {
    input_args$floors <- floors
  } else {
    stop(paste("Invalid parameter 'floors' -->", floors))
  }

  if (missing(year)) {
    stop("Mandatory parameter 'year' is missing")
  } else if (is.null(year)) {
    stop("Mandatory parameter 'year' is NULL")
  } else if (is.na(year)) {
    stop("Mandatory parameter 'year' is NA")
  } else if (is.numeric(year) & year > 0) {
    input_args$year <- year
  } else {
    stop(paste("Invalid parameter 'year' -->", year))
  }

  if (missing(utilisation_key)) {
    stop("Mandatory parameter 'utilisation_key' is missing")
  } else if (is.null(utilisation_key)) {
    stop("Mandatory parameter 'utilisation_key' is NULL")
  } else if (is.na(utilisation_key)) {
    stop("Mandatory parameter 'utilisation_key' is NA")
  } else if (is.numeric(utilisation_key) & utilisation_key %in% unlist(lapply(utilisation, "[[", "key"))) {
    input_args$utilisation_key <- utilisation_key
  } else {
    stop(paste("Invalid parameter 'utilisation_key' -->", utilisation_key))
  }

  if (missing(climate_code)) {
    stop("Mandatory parameter 'climate_code' is missing")
  } else if (is.null(climate_code)) {
    stop("Mandatory parameter 'climate_code' is NULL")
  } else if (is.na(climate_code)) {
    stop("Mandatory parameter 'climate_code' is NA")
  } else if (is.character(climate_code) & climate_code %in% unlist(lapply(climate, "[[", "code"))) {
    input_args$climate_code <- climate_code
  } else {
    stop(paste("Invalid parameter 'climate_code' -->", climate_code))
  }

  if (missing(energy_carrier)) {
    stop("Mandatory parameter 'energy_carrier' is missing")
  } else if (is.null(energy_carrier)) {
    stop("Mandatory parameter 'energy_carrier' is NULL")
  } else if (is.na(energy_carrier)) {
    stop("Mandatory parameter 'energy_carrier' is NA")
  } else if (is.character(energy_carrier) & energy_carrier %in% c("oilHeating", "gasHeating", "other", "undefined")) {
    input_args$energy_carrier <- energy_carrier
  } else {
    stop(paste("Invalid parameter 'energy_carrier' -->", energy_carrier))
  }

  if (!is.null(walls_refurb_year)) {
    if (!is.na(walls_refurb_year)) {
      if (is.numeric(walls_refurb_year) & walls_refurb_year > 0) {
        input_args_refurbishments <- append(input_args_refurbishments, list(list(measure = "walls", year = walls_refurb_year)))
      } else {
        stop(paste("Invalid parameter 'walls_refurb_year' -->", walls_refurb_year))
      }
    }
  }

  if (!is.null(roof_refurb_year)) {
    if (!is.na(roof_refurb_year)) {
      if (is.numeric(roof_refurb_year) & roof_refurb_year > 0) {
        input_args_refurbishments <- append(input_args_refurbishments, list(list(measure = "roof", year = roof_refurb_year)))
      } else {
        stop(paste("Invalid parameter 'roof_refurb_year' -->", roof_refurb_year))
      }
    }
  }

  if (!is.null(windows_refurb_year)) {
    if (!is.na(windows_refurb_year)) {
      if (is.numeric(windows_refurb_year) & windows_refurb_year > 0) {
        input_args_refurbishments <- append(input_args_refurbishments, list(list(measure = "windows", year = windows_refurb_year)))
      } else {
        stop(paste("Invalid parameter 'windows_refurb_year' -->", windows_refurb_year))
      }
    }
  }

  if (!is.null(floor_refurb_year)) {
    if (!is.na(floor_refurb_year)) {
      if (is.numeric(floor_refurb_year) & floor_refurb_year > 0) {
        input_args_refurbishments <- append(input_args_refurbishments, list(list(measure = "floor", year = floor_refurb_year)))
      } else {
        stop(paste("Invalid parameter 'floor_refurb_year' -->", floor_refurb_year))
      }
    }
  }

  if (length(input_args_refurbishments) > 0) {
    input_args$refurbishments <- input_args_refurbishments
  }

  if (!is.null(heating_install_year)) {
    if (!is.na(heating_install_year)) {
      if (is.numeric(heating_install_year) & heating_install_year > 0) {
        input_args$yearInstalledHeating <- heating_install_year
      } else {
        stop(paste("Invalid parameter 'heating_install_year' -->", heating_install_year))
      }
    }
  }

  # Get parameters according to the input arguments
  params <- getParams(input_args)
  return(params)
}
