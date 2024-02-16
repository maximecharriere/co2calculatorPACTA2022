#  Author: Diego Sandoval
#  E-Mail: Sandoval@BS2.ch
#  Implementation of SIA formulas

DAYSINMONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

Q_Total_Sum <- function(params) {
  return(sum(Q_Total(params)))
}

Q_DHW_Demand_Sum <- function(params) {
  return(sum(Q_DHW_Demand(params)))
}

Q_Losses_Total_Sum <- function(params) {
  return(sum(Q_Losses_Total(params)))
}

Q_Gains_Used_Sum <- function(params) {
  return(sum(Q_Gains_Used(params)))
}

Q_Heating_Demand_Sum <- function(params) {
  return(sum(Q_Heating_Demand(params)))
}

#' Calculate Transmission Losses for Building Elements
#'
#' `calculateTransmissionLosses` computes the heat transmission losses through opaque and window elements
#' of a building given the outside temperature and building parameters. This function calculates the losses
#' for each element in watts, taking into account the difference between room temperature (adjusted for
#' specific utilisation) and outside temperature, the area of each element, its U-value, and an optional
#' factor that adjusts the loss calculation.
#'
#' @param outsideTemp Numeric, the outside temperature in degrees Celsius.
#' @param params A list containing parameters and characteristics of the building. This list must include
#'   `utilisation`, `opaqueElements`, and `windowElements`, where:
#'   \describe{
#'     \item{utilisation}{The database with information specific to each building utilisation.}
#'     \item{opaqueElements}{A list of opaque building elements (e.g., walls, roof, floor).}
#'     \item{windowElements}{A list of window elements.}
#'   }
#'
#' @return A list of elements, where each element is a list containing:
#'   \describe{
#'     \item{name}{The name of the building element.}
#'     \item{losses}{Calculated heat transmission losses through the element in watts.}
#'   }
#'
#' @details This function provides a detailed breakdown of transmission losses, useful for thermal
#' analysis of buildings. It differentiates between opaque and window elements, allowing for a
#' comprehensive assessment of the building's thermal envelope.
#'
#' @export
calculateTransmissionLosses <- function(outsideTemp, params) {
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

# TODO
Q_Total <- function(params) {
  dhwDemand <- Q_DHW_Demand(params) # vector
  heatingdemand <- Q_Heating_Demand(params) # vector
  result <- heatingdemand + dhwDemand # vector
  return(result)
}

#' Calculate Monthly DHW Energy Demand per Square Meter
#'
#' `Q_DHW_Demand` estimates the monthly energy required for Domestic Hot Water (DHW)
#' per square meter of the building. The function calculates this demand based on
#' the daily DHW demand specific to the building type.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the monthly energy demand
#'   for DHW per square meter, measured in megajoules (MJ). Each element of the
#'   vector corresponds to one month of the year.
#'
#' @export
Q_DHW_Demand <- function(params) {
  # returns 12-month vector
  result <- (params$utilisation$dhwDemand / 365) * DAYSINMONTH
  return(result)
}

#' Calculate Monthly Transmission Heat Losses in MJ/m2
#'
#' `Q_Losses_Transmission` calculates the heat transmission losses through the building envelope for each month,
#' converting the results into megajoules per square meter (MJ/m2). The function applies formulas 76-96 of the SIA 380/1 norm,
#' to assess losses based on varying outside temperatures throughout the year. It utilizes the
#' \code{\link{calculateTransmissionLosses}} function to compute losses for each building element (opaque and window elements),
#' then aggregates these losses to provide a monthly total.
#'
#' @param params A list containing building parameters and climate data, including:
#'   \describe{
#'     \item{area}{The total area of the building in square meters.}
#'     \item{climate}{The database with the local climate info.}
#'     \item{Other parameters required by \code{\link{calculateTransmissionLosses}}, such as the `utilisation` database, `opaqueElements`, and `windowElements`.}
#'   }
#'
#' @return A numeric vector of length 12, representing the total transmission heat losses for each month,
#'   expressed in MJ/m2. Negative loss values are set to 0 to ensure that all output values represent actual heat losses.
#'   This result aligns with `Q_T` as referred to in Formula 76-96 of the SIA 380/1 norm.
#'
#' @export
Q_Losses_Transmission <- function(params) {
  ## Calculate array of months containing losses for each element in MJ/m2
  transmissionlosses_elements <- list()
  area <- params$area
  for (iMonth in 1:12) {
    monthduration <- DAYSINMONTH[iMonth] * 60 * 60 * 24 # seconds
    outsideTemp <- params$climate$monthlyTemperatureAvg[[iMonth]]
    pl <- calculateTransmissionLosses(outsideTemp, params)

    for (i in 1:length(pl)) {
      pl[[i]]$losses <- pl[[i]]$losses * monthduration / 1000000 / area # W -> MJ/m2
    }
    transmissionlosses_elements[[iMonth]] <- pl
  }

  ## Sum up losses of all elements
  transmissionlosses <- c()
  for (iMonth in 1:12) {
    losses <- 0
    for (el in transmissionlosses_elements[[iMonth]]) {
      losses <- losses + el$losses
    }
    transmissionlosses <- c(transmissionlosses, losses) # vector of 12 months
  }

  ## Set negative values to 0
  transmissionlosses[transmissionlosses < 0] <- 0

  return(transmissionlosses)
}

#' Calculate Monthly Ventilation Heat Losses in MJ/m2
#'
#' `Q_Losses_Ventilation` calculates the heat losses due to ventilation for each month,
#' expressed in megajoules per square meter (MJ/m2). The calculation is based on Formulas 97-98
#' of the SIA 380/1 norm, taking into account the difference between indoor and outdoor temperatures,
#' the building's ventilation rate, and the specific heat capacity of air at a given altitude.
#'
#' @param params A list containing building parameters and climate data, including:
#'   \describe{
#'     \item{utilisation}{The database with information specific to each building utilisation.}
#'     \item{climate}{The database with the local climate info.}
#'   }
#'
#' @return A numeric vector of length 12, representing the ventilation heat losses for each month,
#'   expressed in MJ/m2. Negative loss values are automatically set to 0 to ensure that all output values
#'   represent actual heat losses. This result aligns with `Q_V` as referred
#'   to in Formula 97-98 of the SIA 380/1 norm.
#'
#' @export
Q_Losses_Ventilation <- function(params) {
  ## Calculate ventilation losses in MJ/m2
  roomTemp <- params$utilisation$temperature + params$utilisation$temperatureAddition
  outsideTemp <- params$climate$monthlyTemperatureAvg[1:12] # 13th value is the year-average
  rho_ca <- 1220 - 0.14 * params$climate$altitude # Formula 97 in J/(m3*K)
  ventilationlosses <- (roomTemp - outsideTemp) * params$utilisation$ventilationRate * DAYSINMONTH * 24 * rho_ca / 1000000

  ## Set negative values to 0
  ventilationlosses[ventilationlosses < 0] <- 0
  return(ventilationlosses) # vector of 12 months
}

#' Calculate Total Monthly Heat Losses
#'
#' `Q_Losses_Total`, based on Formula 99 of the SIA 380/1 norm, calculates the total monthly heat losses
#' per square meter of the building. This function combines heat losses due to transmission
#' (\code{\link{Q_Losses_Transmission}}) and ventilation (\code{\link{Q_Losses_Ventilation}}),
#' providing a comprehensive view of the building's overall heat loss. The calculation reflects
#' the sum of these two components, giving an estimate of the total energy loss through the building
#' envelope and due to air exchange.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function, containing
#'   all necessary details for the calculation of total heat losses. This includes building
#'   characteristics, climate data, and specifications of the building's envelope and ventilation system.
#'
#' @return A numeric vector of length 12, representing the total heat losses for each month,
#'   measured in megajoules (MJ) per square meter. Each element corresponds to one month of the year,
#'   illustrating the seasonal fluctuation in heat losses. This result aligns with `Q_ot` as referred
#'   to in Formula 99 of the SIA 380/1 norm.
#'
#' @export
Q_Losses_Total <- function(params) {
  transmissionlosses <- Q_Losses_Transmission(params)
  ventilationlosses <- Q_Losses_Ventilation(params)
  result <- transmissionlosses + ventilationlosses
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_iEl
# Formula (101)
# TODO
Q_Gains_Electricity <- function(params) {
  # Electric gains in MJ/m2
  result <- params$utilisation$electricityUse * params$utilisation$electricityUseFactor * DAYSINMONTH / 365
  return(result)
}

# Returns array of monthly values in MJ/m2
# Q_iP
# Formula (102)
# TODO
Q_Gains_BodyHeat <- function(params) {
  result <- params$utilisation$personHeat * params$utilisation$personPresence * DAYSINMONTH * 60 * 60 / params$utilisation$personArea / (1000000)
  return(result)
}

# Returns array of months containing values for each element in MJ/m2
# Q_sX
# Formula (104) ... (108)
# TODO
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
# TODO
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
# TODO
Q_Gains_Total <- function(params) {
  electricgains <- Q_Gains_Electricity(params)
  bodygains <- Q_Gains_BodyHeat(params)
  solargains <- Q_Gains_Solar_Sum(params)
  result <- electricgains + bodygains + solargains
  return(result)
}

# H
# Formula (100)
# TODO
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
# TODO
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
    } else if (totallosses[iMonth] <= 0) {
      utilisation <- 0
    }
    result <- c(result, totalgains[iMonth] * utilisation)
  }
  return(result)
}

#' Calculate Monthly Heating Energy Demand
#'
#' `Q_Heating_Demand` computes the net energy demand for heating on a monthly basis,
#' per square meter of the building, as outlined in Formula 117 of the SIA 380/1 norm.
#' This function calculates the heating demand (`Q_h` in the formula) by assessing the
#' difference between total heat losses and gains. It utilizes \code{\link{Q_Losses_Total}}
#' for total heat losses and \code{\link{Q_Gains_Used}} for usable heat gains, following
#' the methodology specified by the SIA 380/1 norm.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function,
#'   containing all necessary details for the calculation of heating demand,
#'   including building characteristics, climate data, and energy system parameters.
#'
#' @return A numeric vector of length 12, representing the net heating energy demand
#'   for each month, measured in megajoules (MJ) per square meter. Each element of
#'   the vector corresponds to one month of the year, providing a detailed view of
#'   the heating demand's seasonal variation. This value corresponds to `Q_h` as
#'   referred to in Formula 117 of the SIA 380/1 norm.
#'
#' @export
Q_Heating_Demand <- function(params) {
  totallosses <- Q_Losses_Total(params)
  totalgains <- Q_Gains_Used(params)
  result <- totallosses - totalgains
  return(result) # vector of 12 months
}

#' Calculate GHG Emissions from Energy Use
#'
#' `getEmissions` calculates the greenhouse gas (GHG) emissions associated with the energy use of a building,
#' taking into account the energy carrier, heating efficiency, and total heat energy required.
#' The function considers the efficiency of heating systems installed in different periods and adjusts
#' the emission coefficient accordingly. The calculation is based on the total energy demand as estimated
#' by the `Q_Total_Sum` function and applies conversion factors to translate energy use into GHG emissions.
#'
#' @param params A list containing the parameters of the building and its energy system.
#'   This includes the area of the building, the year of construction, details of the energy carrier,
#'   and information about the heating system. The list is expected to have specific structure and fields
#'   as defined by the package's data model, including `yearInstalledHeating`, `energy_carrier`, `area`,
#'   and `year`.
#'
#' @return \code{getEmissions} returns a list \code{r} containing the calculation results:
#'   \itemize{
#'     \item{r$heatEnergy: annual heat energy required per area, in MJ/m2 per year}
#'     \item{r$emissionCoefficient: applied CO2 emission coefficient according to the given energy carrier, in kg/MJ}
#'     \item{r$emissionsPerArea: annual CO2 emissions per area, in kg/m2 per year}
#'     \item{r$emissionsTotal:  }
#'   }
#'
#' @export
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
#'   energy_carrier = "gasHeating"
#' )
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
#'   heating_install_year = 2010
#' )
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
