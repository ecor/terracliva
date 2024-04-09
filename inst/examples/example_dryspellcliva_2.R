
library(magrittr)
library(terra)
library(terracliva)
library(lubridate)

source("/home/ecor/local/rpackages/jrc/terracliva/R/dryspellcliva.R") ###, echo=TRUE)

years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()


prec <- as.numeric(dataset_daily[100])
timeprec <- time(dataset_daily)
##cc <- system.time({
out <- dryspellcliva(prec,timeprec)
##})

out2 <- dryspellcliva(prec,timeprec,valmin=2)


out5 <- dryspellcliva(prec,timeprec,valmin=5)