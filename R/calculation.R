#  Author: Diego Sandoval
#  E-Mail: Sandoval@BS2.ch
#  Implementation of SIA formulas

DAYSINMONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

Q_Total_Sum <- function(params) {
  return(sum(Q_Total(params)))
}

# Q_DHW_Demand_Sum <- function(params) {
#   return(sum(Q_DHW_Demand(params)))
# }

# Q_Losses_Total_Sum <- function(params) {
#   return(sum(Q_Losses_Total(params)))
# }

# Q_Gains_Used_Sum <- function(params) {
#   return(sum(Q_Gains_Used(params)))
# }

# Q_Heating_Demand_Sum <- function(params) {
#   return(sum(Q_Heating_Demand(params)))
# }

#' Calculate Transmission Losses for Building Elements
#'
#' `calculateTransmissionLosses` computes the heat transmission losses through opaque and window elements
#' of a building given the outside temperature and building parameters. This function calculates the losses
#' for each element in watts, taking into account the difference between room temperature (adjusted for
#' specific utilisation) and outside temperature, the area of each element, its U-value, and an optional
#' factor that adjusts the loss calculation.
#'
#' @param outsideTemp Numeric, the outside temperature in degrees Celsius.
#' @param params A list returned by the \code{\link{get_parameters}} function. This list must include
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

#' Calculate Total Monthly Heat Demand
#'
#' `Q_Total` computes the total monthly heat demand for a building, combining both the heating demand
#' for space heating and the domestic hot water (DHW) demand.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the total heat demand for each month, measured in
#'   megajoules per square meter (MJ/m²). The result reflects the sum of space heating demand and DHW
#'   demand for each month, accounting for seasonal variations in energy requirements.
#'
#' @export
Q_Total <- function(params) {
  dhwDemand <- Q_DHW_Demand(params) # vector
  heatingDemand <- Q_Heating_Demand(params) # vector
  result <- heatingDemand + dhwDemand # vector
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
#' The function applies formulas 76-96 of the SIA 380/1 norm, and aligns with varaible `Q_T`. \cr
#' `Q_Losses_Transmission` calculates the heat transmission losses through the building envelope for each month,
#' converting the results into megajoules per square meter (MJ/m2). It assess losses based on varying outside temperatures throughout the year. It utilizes the
#' \code{\link{calculateTransmissionLosses}} function to compute losses for each building element (opaque and window elements),
#' then aggregates these losses to provide a monthly total.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the total transmission heat losses for each month,
#'   expressed in MJ/m2. Negative loss values are set to 0 to ensure that all output values represent actual heat losses.
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
#' The function applies formulas 97-98 of the SIA 380/1 norm, and aligns with varaible `Q_V`. \cr
#' `Q_Losses_Ventilation` calculates the heat losses due to ventilation for each month,
#' expressed in megajoules per square meter (MJ/m2). It takes into account the difference between indoor and outdoor temperatures,
#' the building's ventilation rate, and the specific heat capacity of air at a given altitude.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the ventilation heat losses for each month,
#'   expressed in MJ/m2. Negative loss values are automatically set to 0 to ensure that all output values
#'   represent actual heat losses.
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
#' The function applies formula 99 of the SIA 380/1 norm, and aligns with varaible `Q_ot`. \cr
#' `Q_Losses_Total` calculates the total monthly heat losses
#' per square meter of the building. This function combines heat losses due to transmission
#' (\code{\link{Q_Losses_Transmission}}) and ventilation (\code{\link{Q_Losses_Ventilation}}),
#' providing a comprehensive view of the building's overall heat loss. The calculation reflects
#' the sum of these two components, giving an estimate of the total energy loss through the building
#' envelope and due to air exchange.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the total heat losses for each month,
#'   measured in megajoules (MJ) per square meter. Each element corresponds to one month of the year,
#'   illustrating the seasonal fluctuation in heat losses.
#'
#' @export
Q_Losses_Total <- function(params) {
  transmissionlosses <- Q_Losses_Transmission(params)
  ventilationlosses <- Q_Losses_Ventilation(params)
  result <- transmissionlosses + ventilationlosses
  return(result)
}

#' Calculate Monthly Electrical Heat Gains
#'
# The function applies formula 101 of the SIA 380/1 norm, and aligns with varaible `Q_iEl`.\cr
#' `Q_Gains_Electricity` computes the monthly heat gains from electrical equipment and appliances
#' within a building, expressed in megajoules per square meter (MJ/m²).
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the electrical heat gains for each month,
#'   calculated based on the building's annual electricity use and the electricity use factor.
#'   The gains are normalized to the building's area and adjusted for the number of days in each month,
#'   providing a monthly breakdown of heat gains from electrical sources.
#'
#' @export
Q_Gains_Electricity <- function(params) {
  # Electric gains in MJ/m2
  result <- params$utilisation$electricityUse * params$utilisation$electricityUseFactor * DAYSINMONTH / 365
  return(result)
}

#' Calculate Monthly Body Heat Gains
#'
#' The function applies formula 102 of the SIA 380/1 norm, and aligns with varaible `Q_iP`.\cr
#' `Q_Gains_BodyHeat` estimates the monthly heat gains attributed to occupants within a building,
#' expressed in megajoules per square meter (MJ/m²). This function calculates the heat generated
#' by the human body, considering the time spent by a person in the building, their heat emission rate, and
#' the occupied area.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the heat gains from occupants for each month,
#'   calculated based on the heat emission rate per person, the density of occupants, and the total
#'   occupied area. The gains are normalized to the building's area and adjusted for the number of days
#'   in each month, providing a monthly breakdown of body heat contributions to the building's internal gains.
#'
#' @export
Q_Gains_BodyHeat <- function(params) {
  result <- params$utilisation$personHeat * params$utilisation$personPresence * DAYSINMONTH * 60 * 60 / params$utilisation$personArea / (1000000)
  return(result)
}

#' Calculate Monthly Solar Heat Gains
#'
#' The function applies formula 104 to 109 of the SIA 380/1 norm, and aligns with varaible `Q_sX` and `Q_s`. \cr
#' `Q_Gains_Solar` estimates the solar heat gains through windows for each month, expressed in
#' megajoules per square meter (MJ/m²). It calculates the solar gains by considering the
#' orientation, area, and specific properties of each window element, along with the monthly solar
#' irradiation. The calculation accounts for factors such as the window's opacity, frame factor, and
#' shading factor, normalizing the gains to the building's area.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the total solar heat gains for each month,
#'   calculated for the entire building and normalized per square meter. Each element of the vector
#'   corresponds to one month, reflecting the impact of solar irradiation variability throughout the year.
#'
#' @export
Q_Gains_Solar <- function(params) {
  solargains_el <- list()
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
    solargains_el[[iMonth]] <- elements
  }

  # Sum up gains of all elements
  # Formula (109) Variable Q_s
  solargains <- c()
  for (iMonth in 1:12) {
    total <- 0
    for (g in solargains_el[[iMonth]]) {
      total <- g$gains + total
    }
    solargains[iMonth] <- total
  }
  return(solargains)
}

#' Calculate Total Monthly Heat Gains
#'
# The function applies formula 110 of the SIA 380/1 norm, and aligns with varaible `Q_g`.\cr
#' `Q_Gains_Total` aggregates the total heat gains within a building per square meter,
#' combining electrical gains, body heat gains, and solar gains.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the total heat gains for each month,
#'   measured in megajoules per square meter (MJ/m²).
#'
#' @export
Q_Gains_Total <- function(params) {
  electricgains <- Q_Gains_Electricity(params)
  bodygains <- Q_Gains_BodyHeat(params)
  solargains <- Q_Gains_Solar(params)
  result <- electricgains + bodygains + solargains
  return(result)
}

#' Calculate Building's Heat Transfer Coefficient
#'
#' The function applies formula 100 of the SIA 380/1 norm, and aligns with varaible `H`.\cr
#' `Heat_Transfer_Coefficient` computes the overall heat transfer coefficient of a building.
#' This coefficient is a measure of the building's ability to
#' transfer heat through its envelope, including both opaque elements (walls, roof, floor)
#' and the effect of ventilation. The calculation takes into account the U-values of the
#' building elements, their areas, and any adjustment factors, as well as the impact of
#' ventilation based on the building's volume and the specific heat capacity of air at the
#' building's altitude.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return Numeric. The overall heat transfer coefficient of the building, expressed in Watts per degree
#'   Celsius (W/°C). This value provides a comprehensive measure of the building's thermal
#'   transmittance, incorporating both conduction through the envelope and convection via ventilation.
#'
#' @export
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

#' Calculate Utilized Monthly Heat Gains
#'
#' The function applies formulas 111-115 of the SIA 380/1 norm, and aligns with varaible `Q_ug`.\cr
#' `Q_Gains_Used` computes the amount of heat gains that are effectively used within the building,
#' taking into account the total potential heat gains (from both internal and external sources)
#' and the building's ability to utilize these gains, based on its thermal characteristics.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the utilized heat gains for each month,
#'   measured in megajoules (MJ) per square meter. The calculation adjusts for the building's
#'   thermal response, ensuring that the reported gains are those that effectively contribute
#'   to reducing the heating demand. Each element corresponds to one month of the year,
#'   reflecting the variability of heat gains utilization across different seasons.
#'
#' @export
Q_Gains_Used <- function(params) {
  result <- c()
  # Formula (112)
  # `timeconst` represents the time constant of the building, which is a measure of how quickly the building responds to changes in external temperature. The time constant gives an indication of the time it takes for the building to react to temperature differences, affecting how heat gains are utilized over time. The resulting timeconst is in hours, reflecting how long it takes for the building to equilibrate to a new steady-state temperature after a change in heat gains or losses. A larger timeconst indicates a building with high thermal mass that reacts slowly to temperature changes.
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
#' The function applies formula 117 of the SIA 380/1 norm, and aligns with varaible `Q_h`.\cr
#' `Q_Heating_Demand` computes the net energy demand for heating on a monthly basis,
#' per square meter of the building.
#' This function calculates the heating demand by assessing the
#' difference between total heat losses and gains. It utilizes \code{\link{Q_Losses_Total}}
#' for total heat losses and \code{\link{Q_Gains_Used}} for usable heat gains.
#'
#' @param params A list returned by the \code{\link{get_parameters}} function.
#'
#' @return A numeric vector of length 12, representing the net heating energy demand
#'   for each month, measured in megajoules (MJ) per square meter. Each element of
#'   the vector corresponds to one month of the year, providing a detailed view of
#'   the heating demand's seasonal variation.
#'
#' @export
Q_Heating_Demand <- function(params) {
  totalLosses <- Q_Losses_Total(params)
  usedGains <- Q_Gains_Used(params)
  result <- totalLosses - usedGains
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
#' @param params A list returned by the \code{\link{get_parameters}} function.
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
    emissionCoefficient <- round(energyCarrier$ghgEmissionsPerMJth / efficiencyCoefficent, 4)
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
#'
#' | ID | Name | Description |
#' | --- | --- | --- |
#' | 1 | Wohnen Mehrfamilienhaus | Mehrfamilienhäuser, Alterssiedlungen und -wohnungen, Hotels, Mehrfamilien-Ferienhäuser und Ferienheime, Kinder- und Jugendheime, Tagesheime, Behindertenheime, Drogenstationen, Kasernen, Strafanstalten |
#' | 2 | Wohnen Einfamilienhaus | Ein- und Zweifamilienhäuser, Ein- und ZweifamilienFerienhäuser, Reiheneinfamilienhäuser |
#' | 3 | Verwaltung | private und öffentliche Bürobauten, Schalterhallen, Arztpraxen, Bibliotheken, Ateliers, Ausstellungsbauten, Kulturzentren, Rechenzentren, Fernmeldegebäude, Fernsehgebäude, Filmstudios |
#' | 4 | Schulen | Gebäude für Schulen aller Stufen, Kindergärten und - horte, Schulungsräume, Ausbildungszentren, Kongressgebäude, Labors, Forschungsinstitute, Gemeinschaftsräume, Freizeitanlagen |
#' | 5 | Verkauf | Verkaufsräume aller Art inkl. Einkaufszentren, Messegebäude |
#' | 6 | Restaurants | Restaurants (inkl. Küchen), Cafeterias, Kantinen, Dancings, Diskotheken |
#' | 7 | Versammlungslokale | Theater, Konzertsäle, Kinos, Kirchen, Abdankungshallen, Aulas, Sporthallen mit viel Publikum |
#' | 8 | Spitäler | Spitäler, psychiatrische Kliniken, Krankenheime, Altersheime, Rehabilitationszentren, Behandlungsräume |
#' | 9 | Industrie | Fabrikationsgebäude, Gewerbebauten, Werkstätten, Servicestationen, Werkhöfe, Bahnhöfe, Feuerwehrgebäude |
#' | 10 | Lager | Lagerhallen, Verteilzentren |
#' | 11 | Sportbauten | Turn- und Sporthallen, Gymnastikräume, Tennishallen, Kegelbahnen, Fitnesszentren, Sportgarderoben |
#' | 12 | Hallenbäder | Hallenbäder, Lehrschwimmbecken, Saunagebäude, Heilbäder |
#'
#'   Check the full dataset using \code{utilisation}.
#' @param climate_code Mandatory, string. Abbreviation code of the assigned climate station according to "Auszug aus Merkblatt SIA 2028:2015".
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
