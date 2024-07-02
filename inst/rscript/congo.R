###
###


library(raster)
library(sf)
library(magrittr)
vect_f <-  "/home/ecor/local/data/spatial/sadec/Shapefiles//DemocraticRepublicOfCongo.shp"  %>% st_read()


inputFile = '/home/ecor/local/data/jrc/climsa/enexus/Input/Precipitation/chirps_daily_19812023.nc'  
congo_inputFile = '/home/ecor/local/data/jrc/climsa/enexus/Input/Precipitation/chirps_daily_19812023_congo.nc' ###grd'  


prec <- stack(inputFile) 
prec_congo <- raster::crop(prec,vect_f,filename=congo_inputFile,overwrite=TRUE)

min_prec_congo <- min(prec_congo)

