% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{Q_Gains_Electricity}
\alias{Q_Gains_Electricity}
\title{Calculate Monthly Electrical Heat Gains}
\usage{
Q_Gains_Electricity(params)
}
\arguments{
\item{params}{A list returned by the \code{\link{get_parameters}} function.}
}
\value{
A numeric vector of length 12, representing the electrical heat gains for each month,
calculated based on the building's annual electricity use and the electricity use factor.
The gains are normalized to the building's area and adjusted for the number of days in each month,
providing a monthly breakdown of heat gains from electrical sources.
}
\description{
\code{Q_Gains_Electricity} computes the monthly heat gains from electrical equipment and appliances
within a building, expressed in megajoules per square meter (MJ/m²).
}
