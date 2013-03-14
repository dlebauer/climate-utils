library(ClimateUtils)
files <- dir(pattern = "gatherRHT_")
for(file in files){
  source(file)
  rm(list = ls())
  gc()
}

load("../data/SE.RData")
load("../data/NW.RData")
load("../data/SW.RData")
load("../data/NE.RData")
long.data <- rbind(NE,SE,SW,NW)
save(long.data, file = "../data/NCEP_VPD.RData")



