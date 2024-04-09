
library(terracliva)
library(magrittr)
library(terra)
source("~/local/rpackages/jrc/terracliva/R/dryspellcliva.R")
source("~/local/rpackages/jrc/terracliva/R/dryspellapprast.R")

years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()


#out <- dryspellapprast(dataset_daily)
out5 <- dryspellapprast(dataset_daily,valmin=5)
out10 <- dryspellapprast(dataset_daily,valmin=10)
