library(magrittr)
library(terra)
library(lmomPi)
library(extRemes)

source("/home/ecor/local/rpackages/jrc/terracliva/R/hwmidcliva.R", echo=TRUE)


years <- 1983:2016
tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% sprintf(tmax_dataset_path,years) %>% rast()

tmax <- as.numeric(tmax_dataset_daily[100])
timex <- time(tmax_dataset_daily)



o_hw <- hwmidcliva(x=tmax,timex=timex)
o_hw6 <- hwmidcliva(x=tmax,timex=timex,start_month=6)
o_hw11 <- hwmidcliva(x=tmax,timex=timex,start_month=11) ## keep attention! Changing the starting months, the final result may be affected!

stop("HERE")
## COLD WAVE 
tmin_dataset_path <- system.file("ext_data/tmin",package="terracliva")
tmin_dataset_daily <- "%s/daily/chirts_daily_goma_tmin_%04d.grd" %>% sprintf(tmin_dataset_path,years) %>% rast()

tmin <- as.numeric(tmin_dataset_daily[100])

o_cw <- hwmidcliva(x=tmin,timex=timex,cold=TRUE)
o_cw2 <- cwmidcliva(x=tmin,timex=timex)
