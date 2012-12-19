library(ClimateUtils)
## SWdec <- get.ncep.vpd(lat.southnorth = c(-65, -1),
##                       lon.westeast = c(-179, -1),
##                       years.minmax = c(1981, 2011),
##                       months.minmax = c(12, 12))
## SWjan <- get.ncep.vpd(lat.southnorth = c(-65, -1),
##                       lon.westeast = c(-179, -1),
##                       years.minmax = c(1982, 2012),
##                       months.minmax = c(1, 1))
## SWfeb <- get.ncep.vpd(lat.southnorth = c(-65, -1),
##                       lon.westeast = c(-179, -1),
##                       years.minmax = c(1982, 2012),
##                       months.minmax = c(2, 2))

## save.image(file = "SW.returns.RData")
## load("SW.returns.RData")
## SW <- data.table(SWdec[, list(id)],
##                  SWdec[,list(latitude)],
##                  SWdec[,list(longitude)],
##                  RH = rowMeans(cbind(SWdec$RH, SWjan$RH, SWfeb$RH)),
##                  T = rowMeans(cbind(SWdec$T, SWjan$T, SWfeb$T)),
##                  VPD = rowMeans(cbind(SWdec$VPD, SWjan$VPD, SWfeb$VPD)))           
## if(any(!dim(SW) == dim(SWdec))) stop("new dimensions of SW are wrong")
## save(SW, file = "../data/SW.RData")


### SE Hemisphere:
## SEdec <- get.ncep.vpd(lat.southnorth = c(-65, -1),
##                       lon.westeast = c(0, 179),
##                       years.minmax = c(1981, 2011),
##                       months.minmax = c(12, 12))
## SEjan <- get.ncep.vpd(lat.southnorth = c(-65, -1),
##                       lon.westeast = c(0, 179),
##                       years.minmax = c(1982, 2012),
##                       months.minmax = c(1, 1))
## SEfeb <- get.ncep.vpd(lat.southnorth = c(-65, -1),
##                       lon.westeast = c(0, 179),
##                       years.minmax = c(1982, 2012),
##                       months.minmax = c(2, 2))
## save.image(file = "SE.returns.RData")
## # load("SE.returns.RData")
## SE <- data.table(SEdec[, list(id)],
##                  SEdec[,list(latitude)],
##                  SEdec[,list(longitude)],
##                  RH = rowMeans(cbind(SEdec$RH, SEjan$RH, SEfeb$RH)),
##                  T = rowMeans(cbind(SEdec$T, SEjan$T, SEfeb$T)),
##                  VPD = rowMeans(cbind(SEdec$VPD, SEjan$VPD, SEfeb$VPD)))
                  
## save(SE, file = "../data/SE.RData")


### NE Hemisphere:
## NE <- get.ncep.vpd(lat.southnorth = c(0, 65),
##                    lon.westeast = c(0, 179),
##                    years.minmax = c(1982, 2012),
##                    months.minmax = c(6, 8))
## save.image(file = "NE.returns.RData") 
## save(NE, file = "../data/NE.RData")
## file.create("NEdownloads-complete")

### NW Hemisphere:
NW <- get.ncep.vpd(lat.southnorth = c(0, 65),
                   lon.westeast = c(-180, -1),
                   years.minmax = c(1982, 2012),
                   months.minmax = c(6, 8))
save(NW, file = "../data/NW.RData")

file.create("NWdownloads-complete")

load("../data/SE.RData")
load("../data/NW.RData")
load("../data/SW.RData")
load("../data/NE.RData")
long.data <- rbind(NE,SE,SW,NW)
save(long.data, file = "../data/NCEP_VPD.RData")


### TESTS of old vs. new
# test <- SWold[SW[key="id"]]
# test <- cbind(test[, list(id, latitude, longitude, VPD, VPDnew = VPD.1)])

# save(test, file = "test.RData")
# load("test.RData")# plots moved to global_vpd_example.Rnw





