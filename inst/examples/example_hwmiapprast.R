library(magrittr)
library(terra)
library(terracliva)
library(tidyterra)
library(sf)
library(tidyterra)
library(ggplot2)
library(colorBlindness)
library(scales)
###library(cblindplot) ## remotes::install_github("ducciorocchini/cblindplot")
years <- 1983:2016

goma <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% st_read()

####
## INSTALLARE ??::https://cran.r-project.org/web/packages/colorBlindness/vignettes/colorBlindness.html

####
tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% sprintf(tmax_dataset_path,years) %>% rast()
tmin_dataset_path <- system.file("ext_data/tmin",package="terracliva")
tmin_dataset_daily <- "%s/daily/chirts_daily_goma_tmin_%04d.grd" %>% sprintf(tmin_dataset_path,years) %>% rast()
#####

result_dir <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/results/hwmid"
###

###


out_hwmid_grd <- "%s/hwmid.grd" %>% sprintf(result_dir)
out_cwmid_grd <- "%s/cwmid.grd" %>% sprintf(result_dir)
cond <- file.exists(out_hwmid_grd) & file.exists(out_cwmid_grd)

if (cond) {
  out_hwmid <- rast(out_hwmid_grd)
  out_cwmid <- rast(out_cwmid_grd)
} else{
  
  out_hwmid <- hwmidapprast(tmax_dataset_daily,filename=out_hwmid_grd,overwrite=TRUE)
  out_cwmid <- cwmidapprast(tmin_dataset_daily,filename=out_cwmid_grd,overwrite=TRUE)
}
###

gghw <- ggplot() + geom_spatraster(data = out_hwmid)+facet_wrap(~lyr)+theme_bw() #, aes(fill = tavg_04))

##gg <- gg+scale_fill_continuous(values=colorRampPalette(Blue2DarkOrange18Steps))
gghw <- gghw+scale_fill_gradient2(high=muted("red"),low=muted("blue"),mid="#FFFFBF",midpoint=0) ###,name="5d_prec_trend",breaks=f00(breaks),label=labels,na.value=NA)
gghw <- gghw+geom_sf(data=goma,fill=NA,color="black")



## COLD WAVE , set minus??? 



ggcw <- ggplot() + geom_spatraster(data = -out_cwmid)+facet_wrap(~lyr)+theme_bw() #, aes(fill = tavg_04))

##gg <- gg+scale_fill_continuous(values=colorRampPalette(Blue2DarkOrange18Steps))
ggcw <- ggcw+scale_fill_gradient2(high=muted("red"),low=muted("blue"),mid="#FFFFBF",midpoint=0) ###,name="5d_prec_trend",breaks=f00(breaks),label=labels,na.value=NA)
ggcw <- ggcw+geom_sf(data=goma,fill=NA,color="black")






