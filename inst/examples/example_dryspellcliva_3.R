library(terracliva)
library(magrittr)
library(terra)


years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()


prec <- as.numeric(dataset_daily[100])
timeprec <- time(dataset_daily)

out <- dryspellcliva(prec,timeprec)
out10 <- dryspellcliva(prec,timeprec,valmin=10)