library(ClimateUtils)
NW <- get.ncep.vpd(lat.southnorth = c(1, 65),
                   lon.westeast = c(-180, -1),
                   years.minmax = c(1982, 2012),
                   months.minmax = c(6, 8))
save(NW, file = "../data/NW.RData")
rm(list = ls())
gc()



