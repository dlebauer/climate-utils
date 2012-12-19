\name{gather}
\alias{gather}
\title{Gather global data}
\usage{
  gather(...)
}
\arguments{
  \item{...}{arguments passed to
  \code{\link{RNCEP::NCEP.gather}}}
}
\value{
  data.table with latitude, longitude, and mean variable
  value at each lat x lon
}
\description{
  Gather Global NCEP data
}
\details{
  Customized implementation of NCEP.gather from the RNCEP
  package
}
\examples{
gather(var = "rhum.sig995", lat.minmax = c(-10, 10), lon.minmax = c(-10, 10), years.minmax = c(2011, 2012), months.minmax = c(6, 8))
}
\author{
  David LeBauer
}

