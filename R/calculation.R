#  Author: Diego Sandoval
#  E-Mail: Sandoval@BS2.ch
#  Implementation of SIA formulas

DAYSINMONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

calculateTransmissionLosses <- function(outsideTemp, params) {
  # outsideTemp: scalar
  # params: object
  # return values in Watts: object
  result <- list()
  roomTemp <- params$utilisation$temperature + params$utilisation$temperatureAddition

  # Opaque elements
  i <- 1
  for (el in params$opaqueElements) {
    factor <- if (exists("el$factor")) el$factor else 1
    element <- list()
    element$name <- el$name
    element$losses <- (roomTemp - outsideTemp) * factor * el$area * el$uValue
    result[[i]] <- element
    i <- i + 1
  }

  # window elements
  for (el in params$windowElements) {
    element <- list()
    element$name <- el$name
    element$losses <- (roomTemp - outsideTemp) * el$area * el$uValue
    result[[i]] <- element
    i <- i + 1
  }
  return(result)
}

Q_Total <- function(params) {
  dhwDemand <- Q_DHW_Demand(params) # vector
  heatingdemand <- Q_Heating_Demand(params) # vector
  result <- heatingdemand + dhwDemand # vector
  return(result)
}

Q_DHW_Demand <- function(params) {
  # returns 12-month vector
  result <- (params$utilisation$dhwDemand / 365) * DAYSINMONTH
  return(result)
}

# Returns array of months containing values for each element in MJ/m2
# Formulas (76) ... (95)
Q_Losses_Transmission0 <- function(params) {
  result <- list()
  area <- params$area
  for (iMonth in 1:12) {
    monthduration <- DAYSINMONTH[iMonth] * 60 * 60 * 24 # seconds
    outsideTemp <- params$climate$monthlyTemperatureAvg[[iMonth]]
    pl <- calculateTransmissionLosses(outsideTemp, params)

    for (i in 1:length(pl)) {
      pl[[i]]$losses <- pl[[i]]$losses * monthduration / 1000000 / area # W -> MJ/m2
    }
    result[[iMonth]] <- pl
  }
  return(result)
}

Q_Total_Sum <- function(params) {
  return(sum(Q_Total(params)))
}

Q_DHW_Demand_Sum <- function(params) {
  return(sum(Q_DHW_Demand(params)))
}

# Returns array of monthly values in MJ/m2
# Intermediary result
Q_Losses_Transmission0_Sum <- function(params) {
  transmissionlosses <- Q_Losses_Transmission0(params)
  result <- c()
  for (iMonth in 1:12) {
    # Sum up losses of all elements
    losses <- 0
    for (l in transmissionlosses[[iMonth]]) {
      losses <- losses + l$losses
    }
    result <- c(result, losses) # vector of 12 months
  }
  return(result)
}

Q_Losses_Ventilation0 <- function(params) {

  # Calculate ventilation losses in MJ/m2
  roomTemp <- params$utilisation$temperature + params$utilisation$temperatureAddition
  outsideTemp <- params$climate$monthlyTemperatureAvg[1:12] # 13th value is the year-average
  rho_ca <- 1220 - 0.14 * params$climate$altitude # Formel 97 in J/(m3*K)
  result <- (roomTemp - outsideTemp) * params$utilisation$ventilationRate * DAYSINMONTH * 24 * rho_ca / 1000000
  return(result) # vector of 12 months
}

# Returns array of monthly values in MJ/m2
# Q_T
# Formula (96)
Q_Losses_Transmission <- function(params) {
  transmissionlosses <- Q_Losses_Transmission0_Sum(params)
  ventilationlosses <- Q_Losses_Ventilation0(params)
  result <- transmissionlosses #+ ventilationlosses
  result[result < 0] <- 0 # set negative values to 0
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_V
# Formula (98)
Q_Losses_Ventilation <- function(params) {
  transmissionlosses <- Q_Losses_Transmission0_Sum(params)
  ventilationlosses <- Q_Losses_Ventilation0(params)
  result <- ventilationlosses
  result[result < 0] <- 0 # set negative values to 0
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_ot
# Formula (99)
Q_Losses_Total <- function(params) {
  transmissionlosses <- Q_Losses_Transmission(params)
  ventilationlosses <- Q_Losses_Ventilation(params)
  result <- transmissionlosses + ventilationlosses
  return(result)
}
Q_Losses_Total_Sum <- function(params) {
  return(sum(Q_Losses_Total(params)))
}

# Returns array of monthly values in MJ/m2
# Q_iEl
# Formula (101)
Q_Gains_Electricity <- function(params) {
  # Electric gains in MJ/m2
  result <- params$utilisation$electricityUse * params$utilisation$electricityUseFactor * DAYSINMONTH / 365
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_iP
# Formula (102)
Q_Gains_BodyHeat <- function(params) {
  result <- params$utilisation$personHeat * params$utilisation$personPresence * DAYSINMONTH * 60 * 60 / params$utilisation$personArea / (1000000)
  return(result)
}

# Returns array of months containing values for each element in MJ/m2
# Q_sX
# Formula (104) ... (108)
Q_Gains_Solar <- function(params) {
  result <- list()
  i <- 1
  for (iMonth in 1:12) {
    elements <- list()
    i <- 1
    for (el in params$windowElements) {
      newel <- list()
      newel$name <- el$name
      newel$gains <- params$climate$monthlyIrradiationSum[[el$orientation]][iMonth] * el$area * 0.9 * el$opacity * el$frameFactor * el$shadingFactor / params$area
      elements[[i]] <- newel
      i <- i + 1
    }
    result[[iMonth]] <- elements
  }
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_s
# Formula (109)
Q_Gains_Solar_Sum <- function(params) {
  solargains <- Q_Gains_Solar(params)

  result <- c()
  for (iMonth in 1:12) {
    # Sum up losses of all elements
    total <- 0
    for (g in solargains[[iMonth]]) {
      total <- g$gains + total
    }
    result[iMonth] <- total
  }
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_g
# Formula (110)
Q_Gains_Total <- function(params) {
  electricgains <- Q_Gains_Electricity(params)
  bodygains <- Q_Gains_BodyHeat(params)
  solargains <- Q_Gains_Solar_Sum(params)
  result <- electricgains + bodygains + solargains
  return(result)
}

# H
# Formula (100)
Heat_Transfer_Coefficient <- function(params) {
  result <- 0
  for (el in params$opaqueElements) {
    if (exists("el$factor")) {
      factor <- el$factor
    } else {
      factor <- 1
    }
    result <- result + el$uValue * el$area * factor
  }
  rho_ca <- 1220 - 0.14 * params$climate$altitude # Formel 97 in J/(m3*K)
  result <- result + params$utilisation$ventilationRate * params$area * rho_ca / 3600
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_ug
# Formula (115)

Q_Gains_Used <- function(params) {
  result <- c()
  # Formula (112)
  timeconst <- params$heatcapacity * params$area * 1000000 / Heat_Transfer_Coefficient(params) / 3600
  # Formula (113)
  utilisationparam <- params$utilisation$heatgainsFactor + (timeconst / params$utilisation$heatgainsTime)
  totalgains <- Q_Gains_Total(params)
  totallosses <- Q_Losses_Total(params)
  for (iMonth in 1:12) {
    # Formula (111)
    ratio <- totalgains[iMonth] / max(1, totallosses[iMonth])

    # Formula (114)
    utilisation <- utilisationparam / (utilisationparam + 1)
    if (ratio != 1) {
      utilisation <- (1 - ratio**utilisationparam) / (1 - ratio**(utilisationparam + 1))
    }
    else if (totallosses[iMonth] <= 0) {
      utilisation <- 0
    }
    result <- c(result, totalgains[iMonth] * utilisation)
  }
  return(result)
}

Q_Gains_Used_Sum <- function(params) {
  return(sum(Q_Gains_Used(params)))
}

Q_Heating_Demand_Sum <- function(params) {
  return(sum(Q_Heating_Demand(params)))
}

# Returns array of monthly values in MJ/m2
# Q_h
# Formula (117)
Q_Heating_Demand <- function(params) {
  totallosses <- Q_Losses_Total(params)
  totalgains <- Q_Gains_Used(params)
  result <- totallosses - totalgains
  return(result) # vector of 12 months
}

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

getEmissions <- function(params) {
  energyCarrier <- constants[[params$energy_carrier]]
  emissionCoefficient <- energyCarrier$ghgEmissionsPerMJth

  # if yearInstalledHeating is NULL or NA, derive from building construction year
  if (is.null(params$yearInstalledHeating) || is.na(params$yearInstalledHeating)) {
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    buildingAge <- max(currentYear - params$year, 0) # building age is >= 0
    heatingAge <- min(buildingAge, 25) # heating age is <= 25
    params$yearInstalledHeating <- currentYear - heatingAge
  }

  # heating efficiency (only applies to oilHeating and gasHeating)
  if (params$energy_carrier %in% c("oilHeating", "gasHeating")) {
    if (params$yearInstalledHeating < 1991) {
      efficiencyCoefficent <- energyCarrier$efficiencyCoefficient$`<1991`
    } else if (params$yearInstalledHeating < 1995) {
      efficiencyCoefficent <- energyCarrier$efficiencyCoefficient$`1991-1994`
    } else if (params$yearInstalledHeating < 2000) {
      efficiencyCoefficent <- energyCarrier$efficiencyCoefficient$`1995-1999`
    } else if (params$yearInstalledHeating < 2010) {
      efficiencyCoefficent <- energyCarrier$efficiencyCoefficient$`2000-2009`
    } else {
      efficiencyCoefficent <- energyCarrier$efficiencyCoefficient$`>2009`
    }
    emissionCoefficient <- round(emissionCoefficient / efficiencyCoefficent, 4)
  } else {
    emissionCoefficient <- 0
  }

  heatEnergy <- Q_Total_Sum(params)
  emissionsPerArea <- heatEnergy * emissionCoefficient
  emissionsTotal <- params$area * emissionsPerArea
  return(list(
    heatEnergy = heatEnergy,
    emissionCoefficient = emissionCoefficient,
    emissionsPerArea = emissionsPerArea,
    emissionsTotal = emissionsTotal
  ))
}

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
  } else if (is.character(energy_carrier) & energy_carrier %in% c("oilHeating", "gasHeating", "other")) {
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
}

#' Calculate CO2 emissions
#'
#' `calculate_emissions` calculates the CO2 emissions of a building according to the input arguments described below.
#'
#' @param area Mandatory, numeric. Area (in square meters) where heating or cooling is required ("Energiebezugsfläche").
#'   \itemize{
#'     \item{e.g. 1000}
#'   }
#' @param floors Mandatory, numeric. Number of floors within the building envelope ("thermische Gebäudehülle").
#'   \itemize{
#'     \item{e.g. 4}
#'   }
#' @param year Mandatory, numeric. Year of building construction.
#'   \itemize{
#'     \item{e.g. 1981}
#'   }
#' @param utilisation_key Mandatory, numeric. Main utilisation of the building according to SIA 380/1:2009.
#'
#'   The following utilisations are possible:
#'   \itemize{
#'     \item{1: Wohnen Mehrfamilienhaus}
#'     \item{2: Wohnen Einfamilienhaus}
#'     \item{3: Büro}
#'     \item{4: Schulen}
#'     \item{5: Verkauf}
#'     \item{6: Restaurants}
#'     \item{7: Versammlungslokale}
#'     \item{8: Spitäler}
#'     \item{9: Industrie}
#'     \item{10: Lager}
#'     \item{11: Sportbauten}
#'     \item{12: Hallenbäder}
#'   }
#'   Check the full dataset using \code{utilisation}.
#' @param climate_code Mandatory, string. Abbreviation code of the assigned climate station according to "Auszug aus Merkblatt SIA 2028:2015".
#'
#'   The following climate stations are available:
#'   \itemize{
#'     \item{ABO: Adelboden}
#'     \item{AIG: Aigle}
#'     \item{ALT: Altdorf}
#'     \item{BAS: Basel-Binningen}
#'     \item{BER: Bern-Liebefeld}
#'     \item{BUS: Buchs-Aarau}
#'     \item{CDF: La Chaux-de-Fonds}
#'     \item{CHU: Chur}
#'     \item{DAV: Davos}
#'     \item{DIS: Disentis}
#'     \item{ENG: Engelberg}
#'     \item{FRE: La Frétaz}
#'     \item{GLA: Glarus}
#'     \item{GSB: Grand-St-Bernard}
#'     \item{GUT: Güttingen}
#'     \item{GVE: Genève-Cointrin}
#'     \item{INT: Interlaken}
#'     \item{KLO: Zürich-Kloten}
#'     \item{LUG: Lugano}
#'     \item{LUZ: Luzern}
#'     \item{MAG: Magadino}
#'     \item{MVE: Montana}
#'     \item{NEU: Neuchâtel}
#'     \item{OTL: Locarno-Monti}
#'     \item{PAY: Payerne}
#'     \item{PIO: Piotta}
#'     \item{PUY: Pully}
#'     \item{ROB: Robbia}
#'     \item{RUE: Rünenberg}
#'     \item{SAM: Samedan}
#'     \item{SBE: San Bernardino}
#'     \item{SCU: Scuol}
#'     \item{SHA: Schaffhausen}
#'     \item{SIO: Sion}
#'     \item{SMA: Zürich-MeteoSchweiz}
#'     \item{STG: St. Gallen}
#'     \item{ULR: Ulrichen}
#'     \item{VAD: Vaduz}
#'     \item{WYN: Wynau}
#'     \item{ZER: Zermatt}
#'   }
#'   Check the full dataset using \code{climate}.
#' @param energy_carrier Mandatory, string. Energy carrier used for heating and domestic hot water production.
#'
#'   The following energy carriers are available:
#'   \itemize{
#'     \item{oilHeating}
#'     \item{gasHeating}
#'     \item{other}
#'   }
#' @param walls_refurb_year Optional, numeric, default value \code{NULL}. Year of wall refurbishment.
#'   \itemize{
#'     \item{e.g. 2010}
#'   }
#'   Supply \code{NULL} or \code{NA} if no refurbishment has taken place since the year of building construction.
#' @param roof_refurb_year Optional, numeric, default value \code{NULL}. Year of roof refurbishment.
#'   \itemize{
#'     \item{e.g. 2010}
#'   }
#'   Supply \code{NULL} or \code{NA} if no refurbishment has taken place since the year of building construction.
#' @param windows_refurb_year Optional, numeric, default value \code{NULL}. Year of window refurbishment.
#'   \itemize{
#'     \item{e.g. 2010}
#'   }
#'   Supply \code{NULL} or \code{NA} if no refurbishment has taken place since the year of building construction.
#' @param floor_refurb_year Optional, numeric, default value \code{NULL}. Year of basement floor refurbishment.
#'   \itemize{
#'     \item{e.g. 2010}
#'   }
#'   Supply \code{NULL} or \code{NA} if no refurbishment has taken place since the year of building construction.
#' @param heating_install_year Optional, numeric, default value \code{NULL}. Year of heating installation.
#'   \itemize{
#'     \item{e.g. 2010}
#'   }
#'   Supply \code{NULL} or \code{NA} if no heating replacement has taken place since the year of building construction.
#'
#' @return \code{calculate_emissions} returns a list \code{r} containing the calculation results:
#'   \itemize{
#'     \item{r$heatEnergy: annual heat energy required per area, in MJ/m2 per year}
#'     \item{r$emissionCoefficient: applied CO2 emission coefficient according to the given energy carrier, in kg/MJ}
#'     \item{r$emissionsPerArea: annual CO2 emissions per area, in kg/m2 per year}
#'     \item{r$emissionsTotal: total annual CO2 emissions, in kg per year}
#'   }
#' @export
#'
#' @examples
#' # Example 1:
#' # calculate CO2 emissions of a multi-family house built in 1981, without refurbishments
#' ex1 <- calculate_emissions(
#'   area = 1000,
#'   floors = 4,
#'   year = 1981,
#'   utilisation_key = 1,
#'   climate_code = "KLO",
#'   energy_carrier = "gasHeating")
#' str(ex1)
#'
#' # Example 2:
#' # same building as above but with additional refurbishments (roof & walls) in 2010
#' ex2 <- calculate_emissions(
#'   area = 1000,
#'   floors = 4,
#'   year = 1981,
#'   utilisation_key = 1,
#'   climate_code = "KLO",
#'   energy_carrier = "gasHeating",
#'   walls_refurb_year = 2010,
#'   roof_refurb_year = 2010,
#'   heating_install_year = 2010)
#' str(ex2)
calculate_emissions <- function(area, floors, year, utilisation_key, climate_code, energy_carrier, walls_refurb_year = NULL, roof_refurb_year = NULL, windows_refurb_year = NULL, floor_refurb_year = NULL, heating_install_year = NULL) {

  # Get parameters
  params <- get_parameters(
    area = area,
    floors = floors,
    year = year,
    utilisation_key = utilisation_key,
    climate_code = climate_code,
    energy_carrier = energy_carrier,
    walls_refurb_year = walls_refurb_year,
    roof_refurb_year = roof_refurb_year,
    windows_refurb_year = windows_refurb_year,
    floor_refurb_year = floor_refurb_year,
    heating_install_year = heating_install_year
  )

  # Calculate CO2 emissions
  result <- getEmissions(params)

  # Return result (as list)
  return(result)
}
