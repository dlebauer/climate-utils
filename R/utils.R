##' VPD Calculator
##'
##' Meta-wrapper to calculate climatological mean values of VPD
##' @title VPD Calculator 
##' @param ... values passed to \code{\link{RNCEP::NCEP.gather}}, e.g. lat.southnorth,  lon.westeast, years.minmax, months.minmax
##' @return data table with latitude, longitude, RH, T, and VPD
##' @author David LeBauer
##' @examples
##' get.ncep.vpd(lat.minmax = c(-10, 10), lon.minmax = c(-10, 10), years.minmax = c(2011, 2012), months.minmax = c(6, 8))
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
##' @export
##' @return data.table with latitude, longitude, and mean variable value at each lat x lon
##' @author David LeBauer
##' @examples
##' gather(var = "rhum.sig995", lat.minmax = c(-10, 10), lon.minmax = c(-10, 10), years.minmax = c(2011, 2012), months.minmax = c(6, 8))
gather <- function(var, ...){
  ans <- NCEP.gather(variable = var,
                     level = "surface",
                     months.minmax = months,
                     years.minmax  = years,
                     lat.southnorth = lat,
                     lon.westeast = lon,
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
  ans.dt2 <- ans.dt[!substr(ans.dt$datetime, 12,13) == "00", 
                    list(date = ymd(substr(datetime, 1, 10)), 
                         latitude, longitude, variable1)]
  ans.dt3 <- ans.dt2[, list(mean = mean(variable1)), by = list(latitude, longitude)] 
  return(ans.dt3)
  rm(list = ls())
  gc()
}


##' Load IBIS output files 
##'
##' @title load.ibis.nc 
##' @param filename name of IBIS output file, netCDF format
##' @param var name of variable to be extracted
##' @param spp name of species (optional)
##' @param date.range either character (one of "all", "summer",
##' "JJA") or integer vector of julian days (e.g. 1:365)
##' of data to extract
##' @param FUN function to be used to summarize data over time
##' @return list of useful data
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{filename = (system.file("extdata/vanloocke2012rcw/corn/aet.nc", package = "ribis"))
##' x <- load.ibis.nc(filename)}
load.ibis.nc <- function(filename = "", var = "",
                         spp = NULL,
                         date.range = "all",
                         FUN = "sum",
                         return.summary = TRUE){

  indata <- open.nc(filename, write=FALSE)

              
  ## Get Coords and Time --- using RNetCDF
  lat  <- var.get.nc(indata, "latitude") # LAT COORDS
  lon  <- var.get.nc(indata, "longitude") # LONG COORDS
  time <- var.get.nc(indata, "time")
  VAR  <- var.get.nc(indata, var)

  ## Coords from VanLoocke et al 2012
  minY <- 37
  maxY <- 46 
  minX <- -104.25
  maxX <- -80.25

  keep.lat <- which(lat >= minY & lat <=maxY)
  keep.lon <- which(lon >= minX & lon <= maxX)
  
  xlab <- paste("annual sum of ",
                paste(sapply(list("long_name", "units"),
                       function(x) att.get.nc(indata, var, x)),
                collapse = " "))
  map.xlab <- att.get.nc(indata, "longitude", "long_name")
  map.ylab <- att.get.nc(indata, "longitude", "long_name")
  main <- paste(spp, toupper(gsub("tot", "", var)), collapse = " ")
  
  ## CONVERT TO GRID AND LOOK AT GRID POINTS
  coords <- expand.grid(x = lon[keep.lon], y = lat[keep.lat])
  
  ## Make Map
  if(date.range %in% c("JJA", "summer")) {
    date.range <- 152:243
  } else if(date.range == "all") {
    date.range <- 1:dim(VAR)[3]
  }
  var.subset <-  VAR[keep.lon, keep.lat, date.range]
  if(return.summary == TRUE){
    var.subset <- apply(var.subset, MARGIN = c(1,2), FUN = FUN)
    
    vect <- as.vector(var.subset)
    array <- data.frame(coords, vect)
    grid_data <- xyz2img(array, tolerance=0.01)
  
    return(list(vect = vect, grid_data = grid_data,
                coords = list(maxX, minX, maxY, minY),
                labels = list(xlab = xlab, main = main,
                  map.xlab = map.xlab, map.ylab = map.ylab, spp = spp)))
  } else if(return.summary == FALSE){
    return(var.subset)
  }
}
##' Get metadata from IBIS netCDF file
##'
##' @title info.ibis.nc 
##' @param filename 
##' @return nothing, print metadata as side effect
##' @export
##' @author David LeBauer
info.ibis.nc <- function(filename) {
  indata <- open.nc(filename, write=FALSE)
  print.nc(indata)
}

##' Create Histogram of IBIS output
##'
##' @title IBIS hist
##' @param ibis.out list of IBIS output returned by \code{\link{load.ibis.nc}}
##' @return histogram
##' @export
##' @author David LeBauer
ibis.hist <- function(ibis.o){
  hist(ibis.o$vect,
       main = ibis.o$labels$main,
       xlab = ibis.o$labels$xlab, ylab = "",
       probability = TRUE, breaks = 60,
       xlim = c(0, max(pretty(ibis.o$vect, na.rm = TRUE))))
  lines(density(ibis.o$vect[!is.na(ibis.o$vect)]))
}
##' from PECAn::sd.var
##'
##' @title sd.var
##' @param x 
##' @return standard deviation of variance estimate
##' @export 
##' @author David LeBauer
sd.var <- function (x) {
    var(x, na.rm = TRUE)^2 * (2/(sum(!is.na(x)) - 1) + kurtosis(x)/sum(!is.na(x)))
  }

##' enhanced version of pairs()
##'
##' @title pairs.cor 
##' @param x 
##' @param y 
##' @param digits 
##' @param prefix 
##' @param cex.cor
##' @export
##' @return plot
##' @author R Development Core Team http://addictedtor.free.fr/graphiques/graphcode.php?graph=137
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
    usr <- par("usr"); on.exit(par(usr)) 
    par(usr = c(0, 1, 0, 1)) 
    r <- abs(cor(x, y)) 
    txt <- format(c(r, 0.123456789), digits=digits)[1] 
    txt <- paste(prefix, txt, sep="") 
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
 
    test <- cor.test(x,y) 
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " ")) 
 
    text(0.5, 0.5, txt, cex = cex * r) 
    text(.8, .8, Signif, cex=cex, col=2) 
}
 
##' Plot IBIS output on a map
##' 
##' @title Plot Map
##' @param output output from \code{\link{load.ibis.nc}} 
##' @param ... additional arguments to image.plot
##' @return map
##' @export
##' @author David LeBauer
plotmap <- function(output,...) {

  if(grepl("AET", output$labels$main, ignore.case = TRUE)){
    zlim <- c(290, 900)
    cols <- wbgyr[169:1]
  } else if (grepl("NPP", output$labels$main, ignore.case = TRUE)){
    zlim <- c(8, 65)
    cols <- wbgyr
  } else if (grepl("SOLAR", output$labels$main, ignore.case = TRUE)){
    zlim <- c(50, 70)
    cols <- wbgyr
  } else if (grepl("RAIN", output$labels$main, ignore.case = TRUE)){
    zlim <- c(290, 1400)
    cols <- wbgyr[169:1]
  } else if (grepl("Clay", output$labels$main, ignore.case = TRUE)){
    zlim <- c(0,50)
    cols <- BlueRedGray
  } else if (grepl("VPD", output$labels$main, ignore.case = TRUE)){
    zlim <- c(1, 7)
    cols <- wbgyr
  } else if (grepl("YIELD", output$labels$main, ignore.case = TRUE)){
    zlim <- c(1, 33)
    cols <- wbgyr
  } else if (grepl("WUE", output$labels$main, ignore.case = TRUE)){
    zlim <- c(3, 60)
    cols <- wbgyr[169:1]
  } else {
    stop("need to set zlim")
  }

  image.plot(output$grid_data,
             col = cols,
             xlim = c(-104.25, -80.25),
             ylim = c(37, 46),
#             xlab = "Longitude",
#             ylab = "Latitude",
             main = paste(output$labels$main, " (",output$labels$xlab,")", sep = ""),
             font.lab=2, 
             cex.lab=1.5,
             legend.shrink = 0.6,
             zlim = zlim,
             ...)
  map("state", add=TRUE, lwd=2,col="grey40")
  box(which = "plot", lty = "solid")
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

