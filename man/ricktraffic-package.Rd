\name{ricktraffic-package}
\alias{ricktraffic-package}
\alias{ricktraffic}
\docType{package}
\title{
A BML Grid Simulation Package
}
\description{
This package is used to simulate BML traffic flow model and provides basic methods for summarizing and visualizing the state of a BML grid.
}
\details{
\tabular{ll}{
Package: \tab ricktraffic\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2013-01-30\cr
License: \tab BSD\cr
}
The major function provided by this package is gridsim(). It accepts certain parameters for the grid, simulates BML process and returns a "CarGrid" object. For detailed information, refer to the help file of function gridsim(). "CarGrid" method for generic functions summary() and plot() can be called for summarizing and visualizing the state of your grid. Additionally the function movesummary() will give information in regards to cars movement between two given time points.
}
\author{
Rick Wang

Maintainer: Rick Wang <erkwang@ucdavis.edu>
}
\keyword{ package }