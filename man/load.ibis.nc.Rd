\name{load.ibis.nc}
\alias{load.ibis.nc}
\title{load.ibis.nc}
\usage{
  load.ibis.nc(filename = "", var = "", spp = NULL,
    date.range = "all", FUN = "sum", return.summary = TRUE)
}
\arguments{
  \item{filename}{name of IBIS output file, netCDF format}

  \item{var}{name of variable to be extracted}

  \item{spp}{name of species (optional)}

  \item{date.range}{either character (one of "all",
  "summer", "JJA") or integer vector of julian days (e.g.
  1:365) of data to extract}

  \item{FUN}{function to be used to summarize data over
  time}
}
\value{
  list of useful data
}
\description{
  Load IBIS output files
}
\examples{
\dontrun{filename = (system.file("extdata/vanloocke2012rcw/corn/aet.nc", package = "ribis"))
x <- load.ibis.nc(filename)}
}
\author{
  David LeBauer
}

