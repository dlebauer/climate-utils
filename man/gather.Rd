\name{gather}
\alias{gather}
\title{Gather global data}
\usage{
  gather(var, lat = c(-90, 90), lon = c(-180, 180),
    years = c(1982, 2012), months = c(6, 8),
    hours = c(6, 18))
}
\arguments{
  \item{var}{variable to gather (see NCEP.gather)}

  \item{lat}{latitude}

  \item{lon}{longitude}

  \item{years}{integer, years to collect data from}

  \item{months}{integer, months to select}

  \item{hours}{currently not implemented}
}
\value{
  data.table with latitude, longitude, and mean variable
  value at each lat x lon
}
\description{
  Customized implementation of NCEP.gather from the RNCEP
  package
}
\author{
  David LeBauer
}

