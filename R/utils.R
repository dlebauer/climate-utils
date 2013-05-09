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

