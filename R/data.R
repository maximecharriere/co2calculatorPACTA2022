#' Climate data.
#'
#' A dataset containing climate data for 40 climate stations in Switzerland.
#'
#' @format A list with 40 elements, each element containing the climate data of the respective station:
#' \itemize{
#'   \item{name}
#'   \item{code}
#'   \item{normTemperature}
#'   \item{latitude}
#'   \item{longitude}
#'   \item{altitude}
#'   \item{climateCode}
#'   \item{situationCode}
#'   \item{monthlyTemperatureAvg}
#'   \item{monthlyIrradiationSum}
#'   \item{PrecipitationSum}
#'   \item{mischungsverhaeltnis}
#'   \item{absFeuchtigkeit}
#' }
#' @source SIA Klimadaten deutsch (kompakt) - Ausgabe 2015, \url{https://www.energytools.ch/index.php/de/downloads/datenbanken}
"climate"

#' Utilisation.
#'
#' Building utilisations according to SIA 380/1:2009.
#'
#' @format A list with 12 elements, each element containing the parameters of the respective utilisation:
#' \itemize{
#'   \item{key}
#'   \item{name}
#'   \item{temperature}
#'   \item{personArea}
#'   \item{personHeat}
#'   \item{personPresence}
#'   \item{electricityUse}
#'   \item{electricityUseFactor}
#'   \item{ventilationRate}
#'   \item{dhwDemand}
#'   \item{temperatureAddition}
#'   \item{heatgainsFactor}
#'   \item{heatgainsTime}
#' }
#' @source Thermische Energie im Hochbau, SIA 380/1:2009, \url{http://www.sia.ch/}
"utilisation"

#' Constants and coefficients.
#'
#' Various constants and coefficients required to calculate the CO2 emissions.
#'
#' @format A list with 10 elements, each element containing the parameters of the respective constant/coefficient:
#' \itemize{
#'   \item{buildingGenerator}
#'   \item{oilHeating}
#'   \item{gasHeating}
#'   \item{other}
#'   \item{dhw_systems}
#'   \item{heating_distribution_systems}
#'   \item{insulation_material}
#'   \item{ventilation_recovery}
#'   \item{insulation_modern}{Not used}
#'   \item{insulation_historical}
#' }
#' @source BS2, \url{https://www.bs2.ch/}
#' @source Wüest Partner, \url{https://www.wuestpartner.com/}
"constants"
