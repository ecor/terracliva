
# R script: enexus_hwmidcliva.R
# Author: Emanuele Cordano
# Description: Heat Waves  terracliva / enexusClimate comparison 
# License (proposed to the custumer): GPL-3.0-or-later

# This script is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This script is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this script. If not, see <https://www.gnu.org/licenses/>.


rm(list=ls())

library(terracliva)
library(lubridate)
library(terra)
library(enexusClimate)
library(raster)
library(rasterList)
library(extRemes)
library(RColorBrewer)
library(testthat)
###source("/home/ecor/local/rpackages/jrc/enexusClimate/R/HeatWaves.R")


years <- 1983:2016
tolerance <- 10^-5
tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% sprintf(tmax_dataset_path,years) %>% rast()



o_hw <- hwmidapprast(tmax_dataset_daily)

#####
#####

spatial_dataset_path <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva")    
spatial_v <- st_as_sfc(st_bbox(tmax_dataset_daily)) %>% as_Spatial() ###spatial_dataset_path %>% st_read() 
inputFile = brick(tmax_dataset_daily+0) ## In memory!!
inputFile <- setZ(inputFile,time(tmax_dataset_daily))
names(inputFile) <- format(time(tmax_dataset_daily),format="%Y.%m.%d")

timerg <- range(time(tmax_dataset_daily))
start_date <- timerg[1]
end_date <- timerg[2]
startDir <- '/home/ecor/local/rpackages/jrc/terracliva_material/enexus_outcomes/hwmid/goma'  
clipByShape = TRUE                             #clip with shapefile boundaries
##shp_utm<-shp_create(spatial_v)
start_year_ref <- year(start_date)
end_year_ref <- year(end_date)

#####
start_date_ref <- as.Date(sprintf("%d-01-01",start_year_ref))
end_date_ref <- as.Date(sprintf("%d-12-31",end_year_ref))
hwmi_thresh=4


cchw <- system.time( hw <- HeatWaves(startDir=startDir, inputFile=inputFile, shp_utm=spatial_v,start_date=as.character(start_date),end_date=as.character(end_date), hwmi_thresh=hwmi_thresh,KtoC=FALSE,start_year_ref=start_year_ref,end_year_ref=end_year_ref,remove0229=FALSE))


####
yys <- sort(unique(year(time(tmax_dataset_daily))))

o_hw_enexus_file <- sprintf('%s/Annual Maps/HWMI_%03d.tif',startDir,yys)
o_hw_enexus <- rast(o_hw_enexus_file)
names(o_hw_enexus) <- str_replace(names(o_hw_enexus),"HWMI_","")

###########
###########
library(ggplot2)




gg <- ggplot()+geom_point(aes(x=o_hw[],y=o_hw_enexus[[names(o_hw)]][]))
gg <- gg+theme_bw()+geom_abline()
gg 
plot(o_hw[],o_hw_enexus[])

tolerance <- 10^-5
exc_difference <- o_hw-o_hw_enexus[[names(o_hw)]]
v <- values(exc_difference)

e <- expect_equal(v,v*0,tolerance=tolerance)




