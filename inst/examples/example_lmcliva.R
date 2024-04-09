library(magrittr)
library(terra)
library(lmomPi)
source("/home/ecor/local/rpackages/jrc/terracliva/R/lmcliva.R")
years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()


prec <- as.numeric(dataset_yearly[100])

out_yearly <- lmcliva(prec)
out_yearly <- lmcliva(prec,distrib="pe3")
ks.test(x=prec,y=cdf,distrib="pe3",para=out_yearly[c(5,6,7)])