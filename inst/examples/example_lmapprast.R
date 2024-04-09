### ** Examples


library(magrittr)
library(terra)
library(lmomPi)

years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()

source("~/local/rpackages/jrc/terracliva/R/lmapprast.R", echo=TRUE)

out_yearly <- lmapprast(dataset_yearly)
library(stringr)
library(lubridate)
dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
time(dataset_monthly) <-  names(dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")
index_monthly  <- month(time(dataset_monthly)) %>% sprintf(fmt="M%02d")

out_monthly <- lmapprast(dataset_monthly,index=index_monthly)