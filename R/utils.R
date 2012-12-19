##' VPD Calculator
##'
##' Meta-wrapper to calculate climatological mean values of VPD
##' @title VPD Calculator 
##' @param ... values passed to \code{\link{RNCEP::NCEP.gather}}, e.g. lat.southnorth,  lon.westeast, years.minmax, months.minmax
##' @return data table with latitude, longitude, RH, T, and VPD
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{get.ncep.vpd(lat.minmax = c(-10, 10), lon.minmax = c(-10, 10), years.minmax = c(2011, 2012), months.minmax = c(6, 8))}
get.ncep.vpd <- function(...){
  temp <- gather(var = "air.sig995", ...) 
  rh   <-  gather(var = "rhum.sig995", ...)

  rh.dt <- ncep2dt(rh)[,list(latitude, longitude, 
                             RH = mean, 
                             id = 1:length(latitude))]
  temp.dt <- ncep2dt(temp)[,list(latitude, longitude, 
                                 T = mean, 
                                 id = 1:length(latitude))]
  setkey(rh.dt, id)
  setkey(temp.dt, id)
  
  long.data <- rh.dt[temp.dt,
                     ][, list(latitude, longitude, RH, T,
                              VPD = get.vpd(RH, T - 272.15)), 
                       by = id]
  return(long.data)
}


##' Gather Global NCEP data
##'
##' Customized implementation of NCEP.gather from the RNCEP package
##' @title Gather global data 
##' @param ... arguments passed to \code{\link{RNCEP::NCEP.gather}}
##' @return data.table with latitude, longitude, and mean variable value at each lat x lon
##' @author David LeBauer
##' @export
##' @examples
##' 
##' gather(var = "rhum.sig995", lat.southnorth = c(-10, 10), lon.westeast = c(-10, 10), years.minmax = c(2011, 2012), months.minmax = c(6, 8))
gather <- function(...){
  ans <- NCEP.gather(...,
                     level = "surface",
                     reanalysis2 = FALSE,
                     return.units = TRUE,
                     status.bar = FALSE)
  return(ans)
  rm(ans); gc()
}

##' converts NCEP.gather output to data table 
##' @title Gather global data 
##' @param ans output from \code{\link{NCEP.gather}} function
##' @export
##' @return data.table with latitude, longitude, and mean
##' @author David LeBauer
ncep2dt <- function(ans){
  ans.dt <- data.table(NCEP.array2df(ans))
  ans.dt2 <- ans.dt[,#!substr(ans.dt$datetime, 12,13) == "00", 
                    list(date = ymd(substr(datetime, 1, 10)), 
                         latitude, longitude, variable1)]
  ans.dt3 <- ans.dt2[, list(mean = mean(variable1)), by = list(latitude, longitude)] 
  return(ans.dt3)
  rm(list = ls())
  gc()
}


##' Calculate VPD
##'
##' Calculate vapor pressure deficit from relative humidity and temperature.
##' @title VPD
##' @param rh relative humidity, in percent 
##' @param temp temperature, degrees celsius
##' @return vpd: vapor pressure deficit, in mb
##' @export
##' @author David LeBauer
##' @examples
##' temp <- -30:30
##' plot(temp, get.vpd(0, temp))
get.vpd <- function(rh, temp){
  ## calculate saturation vapor pressure
  es <- get.es(temp)
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}
##' Calculate saturation vapor pressure
##'
##' @title get es
##' @param temp temperature in degrees C 
##' @return saturation vapor pressure in mb
##' @export
##' @author David LeBauer
##' @examples
##' temp <- -30:30
##' plot(temp, get.es(0, temp))
get.es <- function(temp){
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp)))
  return(es)
}
##' Calculate RH from temperature and dewpoint
##'
##' Based on equation 12 ( in Lawrence 2005, The Relationship between Relative Humidity and the Dewpoint Temperature in Moist Air A Simple Conversion and Applications. 
##' @title get RH
##' @param temp T in original equation
##' @param dewpoint Td in original 
##' @return numeric vector
##' @export
##' @author David LeBauer
get.rh <- function(T, Td){
  arg <- - L / (Rw * T * Td) * (T - Td)
  rh <- 100*exp(- L / (Rw * T * Td) * (T - Td))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Wide to Long
##' @param data.wide data
##' @param lat latitude for rows
##' @param lon longitude for columns
##' @param var variable being measured
##' @return data.frame with colnames (lat, lon, var)
##' @export
##' @author David LeBauer
wide2long <- function(data.wide, lat, lon, var){
  require(reshape)
  colnames(data.wide) <- lon
  data.wide <- cbind(lat, data.wide)
  data.long <- melt(data.wide, id = "lat")
  colnames(data.long) <- c("lat", "lon", var)
  data.long$lon <- as.numeric(as.character(data.long$lon))
  return(data.long)
}

