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
