library(ClimateUtils)

NE <- get.ncep.vpd(lat.southnorth = c(1, 65),
                   lon.westeast = c(1, 180),
                   years.minmax = c(1982, 2012),
                   months.minmax = c(6, 8))
save(NE, file = "../data/NE.RData")



