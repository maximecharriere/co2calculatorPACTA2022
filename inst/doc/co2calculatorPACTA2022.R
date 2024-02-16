## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
# Load package
library(co2calculatorPACTA2022)

# Calculate the CO2 emission of a multi-family house built in 1981, without refurbishments
r <- calculate_emissions(
  area = 1000,
  floors = 4,
  year = 1981,
  utilisation_key = 1,
  climate_code = "KLO",
  energy_carrier = "gasHeating")

# Check result
str(r)

