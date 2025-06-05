library(terracliva)
library(magrittr)
library(terra)
library(lmomPi)
library(terra)
library(raster)

source("~/local/rpackages/jrc/terracliva/R/spiapprast.R")
years <- 1982:2023

dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% 
  sprintf(dataset_path,years) %>% rast()
terra::time(dataset_monthly) <-  names(dataset_monthly) %>% 
  paste0("_01") %>% as.Date(format="X%Y_%m_%d")


spi1 <- spiapprast(x=dataset_monthly,distrib="pe3")
spi1 <- spiapprast(x=dataset_monthly)
spi3 <- spiapprast(x=dataset_monthly,spi.scale=3)

spi1r <- spiapprast(x=dataset_monthly,summary_regress=TRUE)
spi3r <- spiapprast(x=dataset_monthly,summary_regress=TRUE,spi.scale=3)

spi3r_cat <- spiapprast(x=dataset_monthly,spi.scale=3,add_cat=TRUE)