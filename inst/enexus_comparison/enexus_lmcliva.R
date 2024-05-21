
# R script: enexus_lmcliva.R
# Author: Emanuele Cordano
# Description: Monthly Deficit  terracliva / enexusClimate comparison 
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


library(magrittr)
library(terra)
library(lmomPi)
library(lubridate)
library(sf)
library(raster)
library(enexusClimate)
library(testthat)

tolerance <- 10^-5
years <- 1982:2023
##dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
spatial_dataset_path <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva")                          
precipitation_dataset_path <- system.file("ext_data/precipitation",package="terracliva")                               
                               
precipitation_dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(precipitation_dataset_path,years) %>% rast()
time(precipitation_dataset_monthly) <-  names(precipitation_dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")

distrib <- "pe3"
rt_years = c(2,5,10,20,50)
out_monthly <- lmapprast(precipitation_dataset_monthly,index="monthly",distrib=distrib,rt=rt_years)

###########
##

spatial_v <- st_as_sfc(st_bbox(precipitation_dataset_monthly)) ###spatial_dataset_path %>% st_read() 
precmmfile <-  precipitation_dataset_monthly 
precmmfile[precmmfile<0] <- 0
precmmfile[precmmfile>10^4] <- 10^4
mon_vec <- 1:12
timerg <- range(time(precmmfile))
start_date <- timerg[1]
end_date <- timerg[2]
startDir_monthly <- '/home/ecor/local/rpackages/jrc/terracliva_material/enexus_outcomes/lm_monthly/goma'  
################


shp_utm <- spatial_v %>% as_Spatial()
inputFile <- stack(precmmfile)
##
mon_vec <- 1:12
pcts <- c(0,5,10,15,20,30,40,50)
##
cc_monthly <- system.file(out_enexus_monthly <- MonthlyRainDeficit(startDir_monthly, inputFile, shp_utm,start_date=start_date,end_date=end_date,rt_years=rt_years,pcts=pcts,mon_vec=mon_vec))


##################
##################
mon_names <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
mon_numbs <- sprintf("M%02d",1:12)



### Check L-Moments
lmom_files <- list()
nn_names <- list()
lchar <- c("l","l","t","t")
out_lm_enexus <- list()


for (il in 1:length(lchar)) {
  lmom_files[[il]] <- sprintf("%s/L-Moments/%s/Lmoments_%d%s.tif",startDir_monthly,mon_names,il,mon_names)
  nn_names[[il]] <- paste(mon_numbs,lchar[il],il,sep="_")
  lmtmp <- rast(lmom_files[[il]])
  ## l_2 not t_2!!
  if (il==2) {
    
    lmtmp <- lmtmp*out_lm_enexus[[1]]
  }
  names(lmtmp) <- nn_names[[il]] 
  out_lm_enexus[[il]] <- lmtmp
  #####
 
  
  
}

out_lm_enexus <- rast(out_lm_enexus)

lm_difference <- out_monthly[[names(out_lm_enexus)]]-out_lm_enexus
v <- values(lm_difference)

test_lm <- expect_equal(v,v*0,tolerance=tolerance)

## Check Deficit 
def_files <- list()
nn_def_files <- list()
out_def_enexus <- list()
l1_prec <- out_lm_enexus[[which(str_sub(names(out_lm_enexus),start=-3)=="l_1")]]
names(l1_prec) <- sapply(str_split(names(l1_prec),"_"),FUN=function(x){x[1]})
l1_prec <- l1_prec[[mon_numbs]]

for (rt_year in rt_years) {

  rt_year_fmt <- sprintf("%03d",rt_year)
  def_files[[rt_year_fmt]] <- sprintf("%s/Differences/%s/Pdef_Tr%s%sPercentageMap_%s.tif",startDir_monthly,mon_names,rt_year_fmt,mon_names,distrib)
  nn_def_files[[rt_year_fmt]] <- paste(mon_numbs,distrib,"def","rt",rt_year_fmt,sep="_")
  def_tmp <- rast(def_files[[rt_year_fmt]])
  names(def_tmp) <- names(l1_prec)
  ###
  prec_def_tmp <- l1_prec*(1-def_tmp/100)
  names(prec_def_tmp) <- nn_def_files[[rt_year_fmt]]
  out_def_enexus[[rt_year_fmt]] <- prec_def_tmp

}
names(out_def_enexus) <- NULL

out_def_enexus <- rast(out_def_enexus)

def_difference <- out_monthly[[names(out_def_enexus)]]-out_def_enexus
v <- values(def_difference)

expect_equal(v,v*0,tolerance=tolerance)


plot(max(def_difference))
plot(min(def_difference))
###################
###################
###################






## Check Excess 
exc_files <- list()
nn_exc_files <- list()
out_exc_enexus <- list()
l1_prec <- out_lm_enexus[[which(str_sub(names(out_lm_enexus),start=-3)=="l_1")]]
names(l1_prec) <- sapply(str_split(names(l1_prec),"_"),FUN=function(x){x[1]})
l1_prec <- l1_prec[[mon_numbs]]

for (rt_year in rt_years) {
  
  rt_year_fmt <- sprintf("%03d",rt_year)
  exc_files[[rt_year_fmt]] <- sprintf("%s/Differences/%s/Pexc_Tr%s%sPercentageMap_%s.tif",startDir_monthly,mon_names,rt_year_fmt,mon_names,distrib)
  nn_exc_files[[rt_year_fmt]] <- paste(mon_numbs,distrib,"exc","rt",rt_year_fmt,sep="_")
  exc_tmp <- rast(exc_files[[rt_year_fmt]])
  names(exc_tmp) <- names(l1_prec)
  ###
  prec_exc_tmp <- l1_prec*(1+exc_tmp/100)
  names(prec_exc_tmp) <- nn_exc_files[[rt_year_fmt]]
  out_exc_enexus[[rt_year_fmt]] <- prec_exc_tmp
  
}
names(out_exc_enexus) <- NULL

out_exc_enexus <- rast(out_exc_enexus)

exc_difference <- out_monthly[[names(out_exc_enexus)]]-out_exc_enexus
v <- values(exc_difference)

expect_equal(v,v*0,tolerance=tolerance)

plot(max(exc_difference))
plot(min(exc_difference))
###################
###################
###################
gg <- ggplot()+geom_point(aes(x=out_monthly[[names(out_exc_enexus)]][],y=out_exc_enexus[]))
gg <- gg+theme_bw()+geom_abline()
gg 

