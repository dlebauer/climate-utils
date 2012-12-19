
library(ClimateUtils)
temp <- gather("air.sig995", lat=c(0,1), lon = c(0,1), years = c(2010,2011),months=c(6,7))
rh   <-  gather("rhum.sig995")
save(temp, rh, file="NCEP_RH_T.RData")
