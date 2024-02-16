
<!-- README.md is generated from README.Rmd. Please edit that file -->

# co2calculatorPACTA2022 (PACTA 2022)

The `co2calculatorPACTA2022` R package provides the functionality to
calculate the carbon dioxide (CO2) emission of a building. The
calculation is based on the norm SIA 380/1:2009 “Thermische Energie im
Hochbau”.

## Installation

You can install the `co2calculatorPACTA2022` from the source tarball
with:

``` r
install.packages("co2calculatorPACTA2022_1.2.0.tar.gz", repos = NULL, type = "source")
```

## Calculation

In order to calculate the CO2 emissions of a building,
`co2calculatorPACTA2022` provides a single function
`calculate_emissions`.

``` r
# Load package
library(co2calculatorPACTA2022)
#> Package co2calculatorPACTA2022 1.2.0 loaded
#> Need help? --> `vignette("co2calculatorPACTA2022")`

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
#> List of 4
#>  $ heatEnergy         : num 458
#>  $ emissionCoefficient: num 0.064
#>  $ emissionsPerArea   : num 29.3
#>  $ emissionsTotal     : num 29343
```

Please refer to `help("calculate_emissions")` for further documentation
regarding `calculate_emissions`.

## Data

`co2calculatorPACTA2022` includes three datasets. Check their help pages
for further details:

-   `climate`: `help("climate")`
-   `utilisation`: `help("utilisation")`
-   `constants`: `help("constants")`

## Source code

The source tarball contains the complete source code and data of the
`co2calculatorPACTA2022` R package.
