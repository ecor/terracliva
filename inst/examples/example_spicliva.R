library(terracliva)
library(magrittr)
library(terra)
library(lmomPi)

source("~/local/rpackages/jrc/terracliva/R/spicliva.R")
source("~/local/rpackages/jrc/terracliva/R/regress_v2.R")

years <- 1982:2023



dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
terra::time(dataset_monthly) <-  names(dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")

prec <- as.numeric(dataset_monthly[100])
timeprec <- terra::time(dataset_monthly)
spi1 <- spicliva(x=prec,timex=timeprec,distrib="pe3")
spi1 <- spicliva(x=prec,timex=timeprec)
spi3 <- spicliva(x=prec,timex=timeprec,spi.scale=3)

spi1r <- spicliva(x=prec,timex=timeprec,summary_regress=TRUE)
spi3r <- spicliva(x=prec,timex=timeprec,spi.scale=3,summary_regress=TRUE)