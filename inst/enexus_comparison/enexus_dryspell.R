
library(magrittr)
library(terra)
library(enexusClimate)
library(terracliva)

###source("~/local/rpackages/jrc/enexusClimate/R/AnnualDrySpells.R")



years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
months = c(12, 1, 2, 3)
cc <- system.time({
out <- dryspellapprast(dataset_daily,valmin=1,months=months)

})


## out5 <- dryspellapprast(dataset_daily,valmin=5)
## out9 <- dryspellapprast(dataset_daily,valmin=9)


library(enexusClimate)
library(RColorBrewer)

##source("~/local/rpackages/jrc/enexusClimate/R/AnnualDrySpells.R")

startDir = '/home/ecor/local/rpackages/jrc/terracliva_material/enexus_outcomes/dryspell/goma'  
inputFile = dataset_daily  ####"/home/ecor/local/rpackages/jrc/eNexusClimate_material_dryspell/inst/ex_data/chirps_daily_new.nc"
start_date = '1981-01-01'
end_date = '2017-12-31'
clipByShape = FALSE
month = months #6:8 ##c(1,2,3,4,5,6,7,8,9,10,11,12)
aggr_fun_suffixes = c("drySpellCountLow","drySpellCountIntermediate","drySpellCountHigh","drySpellCount","median","max","q25","q75","mean","q90","q90_7","iqr","sum")
min_dry_spell_length = 3




dir.create(startDir)
shp_utm<-  spatial_v <- st_as_sfc(st_bbox(dataset_daily)) ##shp_create(inputShape)

out <- AnnualDrySpells(startDir, inputFile, shp_utm, start_date,end_date, clipByShape, month, aggr_fun_suffixes, min_dry_spell_length)
