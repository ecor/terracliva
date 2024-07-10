
library(magrittr)
library(terra)
library(enexusClimate)
library(terracliva)
library(rasterVis)
###source("~/local/rpackages/jrc/enexusClimate/R/AnnualDrySpells.R")
source("~/local/rpackages/jrc/enexusClimate/R/AnnualDrySpells.R", echo=TRUE)
source("~/local/rpackages/jrc/enexusClimate/R/dateCheck.R", echo=TRUE)

years <- 1982:2022
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
months = c(12, 1, 2, 3,4)
aggr_fun_suffixes = c("drySpellCount_003_010_days","drySpellCount_010_021_days","drySpellCount_021_999_days","drySpellCount","median","max","q25","q75","mean","q90","q90_7","iqr","sum")

filename = '/home/ecor/local/rpackages/jrc/terracliva_material/outcomes/dryspell/goma_dryspell_terracliva.tif'
cond <- file.exists(filename)
##cond <- FALSE

cc <- system.time({
  if (cond) {
    out <- rast(filename)
  } else {
    out <- dryspellapprast(dataset_daily,valmin=1,months=months,fun_aggr=aggr_fun_suffixes,dryspell_starts_in_months=TRUE,dryspell_ends_in_months=TRUE,filename=filename,overwrite=TRUE)
  }

})



library(enexusClimate)
library(RColorBrewer)

##source("~/local/rpackages/jrc/enexusClimate/R/AnnualDrySpells.R")

startDir = '/home/ecor/local/rpackages/jrc/terracliva_material/enexus_outcomes/dryspell/goma'  
inputFile = brick(dataset_daily+0)  ####"/home/ecor/local/rpackages/jrc/eNexusClimate_material_dryspell/inst/ex_data/chirps_daily_new.nc"
inputFile <- setZ(inputFile,time(dataset_daily))
names(inputFile) <- format(time(dataset_daily),format="%Y.%m.%d")

rdates <- range(time(dataset_daily))
start_date <-  rdates[1]
end_date = rdates[2]
clipByShape = FALSE
month = months #6:8 ##c(1,2,3,4,5,6,7,8,9,10,11,12)
###aggr_fun_suffixes = c("drySpellCountLow","drySpellCountIntermediate","drySpellCountHigh","drySpellCount","median","max","q25","q75","mean","q90","q90_7","iqr","sum")
min_dry_spell_length = 3




dir.create(startDir)
shp_utm<-  spatial_v <- st_as_sfc(st_bbox(dataset_daily)) ##shp_create(inputShape)

out2 <- AnnualDrySpells(startDir, inputFile, as_Spatial(shp_utm), start_date,end_date, clipByShape, month, aggr_fun_suffixes, min_dry_spell_length)
out2a <- lapply(out2,FUN=rast) %>% sds() %>% rast()
names(out2a) <- str_replace(names(out2a),"_X","_")
nn <- intersect(names(out),names(out2a))

out_terracliva  <- out[[nn]]
out_enexus <- out2a[[nn]]
## nnf index for aggr fun 


dout <- out_terracliva-out_enexus

dout_3857 <- project(dout,y="epsg:3857",method="near")

plet(dout_3857,y=names(dout_3857))

stop("HERE")


nnf <- str_split(nn,"_") %>% lapply(FUN=function(x){x[-length(x)]}) %>%  sapply(FUN=paste,collapse="_")

de_out <- out_terracliva-out_enexus
de_out_max <- tapp(de_out,fun=max,index=nnf)
de_out_min <- tapp(de_out,fun=min,index=nnf)
###

de_out_max <- tapp(out_terracliva,fun=terracliva::regress,index=nnf)
regress_out_enexus <- tapp(out_enexus,fun=terracliva::regress,index=nnf)

uu <- regress_out_terracliva-regress_out_enexus
