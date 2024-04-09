rm(list=ls())
library(chirps)
library(sf)
library(magrittr)
library(lubridate)
library(terra)
library(stringr)
###
where  <- dsn <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/OSM_Goma_quartiers_210527.shp" %>% st_read()
  
eex <- rast(ext(where)+0.5,crs=st_crs(where))
eex[] <- 0
eex


years <- 1982:1990 #2023

out <- list()

for (year in years) {
  
  filename <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation/daily/chirps_daily_goma_%04d.grd" %>% sprintf(year)
  dates <- c("%d-01-01","%d-12-31") %>% sprintf(year)
  
  cond <- file.exists(filename)
  if (!cond) {
    rr <- get_chirps(object=as.polygons(ext(eex)),dates=dates,server="CHC",as.raster=TRUE)
    time(rr) <- as.Date(dates[1])+days(1:nlyr(rr))-days(1)
    names(rr) <- time(rr)
    rr[rr<0] <- NA
    writeRaster(rr,filename=filename,overwrite=TRUE)
  } 
  out[[as.character(year)]] <- rast(filename)
  ##dates <- c("%d-01-01","%d-12-31") %>% mapply(FUN=sprintf,X=c(start_year,end_year),USE.NAMES=FALSE,SIMPLIFY = TRUE)
  ##cc <- system.time({
  ##rr <- get_chirps(object=as.polygons(ext(eex)),dates=dates,server="CHC",as.raster=TRUE)
  
}

out_monthly <- list()
out_yearly <- list()
for (year in years) {
  
  filename1 <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(year)
  filename2 <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(year)
  cond2 <- file.exists(filename1) 
  ## cond2 <- FALSE
  if (!cond2) {
    rr <- out[[as.character(year)]]
    index1 <- month(time(rr)) %>% sprintf(fmt="%02d") 
    index2 <- year(time(rr))
    index21 <- paste(index2,index1,sep="_")
    
    out_monthly[[as.character(year)]] <- rr %>% tapp(index=index21,filename=filename1,fun=navaluer::sum,overwrite=TRUE)
    out_yearly[[as.character(year)]] <- rr %>% tapp(index=index2,filename=filename2,fun=navaluer::sum,overwrite=TRUE)
    ####
    
    
    
    ####
    
    
    ##writeRaster(rr,filename=filename,overwrite=TRUE)
    
    
  } else {
    
    out_monthly[[as.character(year)]] <- rast(filename1)
    out_yearly[[as.character(year)]] <- rast(filename2)
    ###
    
    ###
  }
  
  time(out_monthly[[as.character(year)]]) <- out_monthly[[as.character(year)]] %>% names() %>% paste0("_01") %>% str_replace_all("[A-Z,a-z]","") %>% as.Date(format="%Y_%m_%d")
  
  
}


out_monthly2 <- rast(out_monthly)
out_yearly2 <- rast(out_yearly)

stop("HERE")








out_monthly2 <- rast(out_monthly)
out_yearly2 <- rast(out_yearly)
time(out_yearly2) <- names(out_yearly2) %>% str_replace_all("[A-Z,a-z]","") %>% as.numeric()


### agreed with Marco Pastori (2022-02-22)
filename <- "/home/ecor/local/data/climate/precipitation/chirps/jrc/mean_2003_2022_yearly_prec_v2.tif"
mean_2003_2022_ye




