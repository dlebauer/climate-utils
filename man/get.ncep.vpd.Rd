\name{get.ncep.vpd}
\alias{get.ncep.vpd}
\title{VPD Calculator}
\usage{
  get.ncep.vpd(...)
}
\arguments{
  \item{...}{values passed to
  \code{\link{RNCEP::NCEP.gather}}, e.g. lat.southnorth,
  lon.westeast, years.minmax, months.minmax}
}
\value{
  data table with latitude, longitude, RH, T, and VPD
}
\description{
  VPD Calculator
}
\details{
  Meta-wrapper to calculate climatological mean values of
  VPD
}
\examples{
\dontrun{get.ncep.vpd(lat.minmax = c(-10, 10), lon.minmax = c(-10, 10), years.minmax = c(2011, 2012), months.minmax = c(6, 8))}
}
\author{
  David LeBauer
}

