library(ClimateUtils)
SW <- get.ncep.vpd(lat.southnorth = c(-65, -1),
                   lon.westeast = c(1, 180),
                   years.minmax = c(1982, 2012),
                   months.minmax = c(12, 2))
save(SW, file = "../data/SW.RData")
