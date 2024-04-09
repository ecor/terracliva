## NULL
## NULL
###https://www.nature.com/articles/s41597-020-0433-7
####https://doi.pangaea.de/10.1594/PANGAEA.909132
library(terra)
library(stringr)
library(magrittr)
library(raster)
library(ncdf4)
library(raster)
library(data.table)
library(dplyr)
wpath <- "/home/ecor/local/data/agriculture/historical_yield/"
if (str_sub(wpath,-1)=="/") wpath <- str_sub(wpath,end=-2)



crops <- c("maize","maize_major"
,"maize_second"
,"rice"  
,"rice_major"  
,"rice_second" 
,"soybean"  
,"wheat"  
,"wheat_spring"  
,"wheat_winter")

### STOP HERE!!!
out <- list()
cond <- TRUE
if (cond) for (crop in crops) {
  
  
  tmp  <- paste(wpath,crop,sep="/") 
  ncs <- tmp %>% list.files(full.names=TRUE,pattern=".nc4") 
  names(ncs) <- tmp %>% list.files(full.names=FALSE,pattern=".nc4") 
  for (it in names(ncs)) {
    
    ncf <- ncs[it]
    rr <- rast(ncf)
    ss <- rr %>% as.data.frame(xy=TRUE,na.rm=FALSE)
    iwest <- which(ss$x>180)
    ss$x[iwest] <- ss$x[iwest]-360
    sr <- rast(ss)
    crs(sr) <- crs(rr)
    nsr <- it %>% str_replace_all("yield_","") %>% str_replace_all(".nc4","") 
    names(sr) <- nsr
    time(sr)  <- as.numeric(nsr)
    
    
    tff <- ncf
    extension(tff) <- ".tif"
   
    sr1 <- writeRaster(sr,filename=tff,overwrite=TRUE) 
  }
  
  
} 

for (crop in crops) {
  
  tmp  <- paste(wpath,crop,sep="/") 
  tff <- tmp %>% list.files(full.names=TRUE,pattern=".nc4") 
  extension(tff) <- ".tif"
  
 ## names(tffs) <- tmp %>% list.files(full.names=FALSE,pattern=".tif") 
  
  tmp <- rast(tff)
 # names(tmp) <- names(tffs)
 #  time(tmp) <- names(tmp) %>% str_replace_all("yield_","") %>% str_replace_all(".tif","") %>% as.numeric()
  out[[crop]] <- tmp
  
  
}

library(terracliva)
library(magrittr)
library(terra)


years <- 1982:2023
###precipipitation_dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
precipitation_dataset_path <- system.file("ext_data/precipitation",package="terracliva")
precipitation_dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(precipitation_dataset_path,years) %>% rast()

############### DRYSPELL
valmins=c(1,3,5,8)
names(valmins) <- sprintf("valmin_%03d",valmins)
months <- c(12,1,2)

drys_grd_format <- "/home/ecor/local/rpackages/jrc/terracliva//inst/ext_data/results/dryspell/drysp_ex0_valmin_%03d.grd"
drys_grd_aggr_format <- "/home/ecor/local/rpackages/jrc/terracliva//inst/ext_data/results/dryspell/drysp_ex0_aggr_valmin_%03d.grd"
drys_grd <- sprintf(drys_grd_format,valmins)
names(drys_grd) <- names(valmins)

drys_grd_aggr <- sprintf(drys_grd_aggr_format,valmins)
names(drys_grd_aggr) <- names(valmins)

drys <- list() 
drys_aggr <- list()
drys_aggr_df <- list()
for (nv in names(valmins)) {
  valmin <- valmins[nv]
  filename <- drys_grd[nv]
  cond <- file.exists(filename)
  ##cond <- FALSE
  if (cond) {
    drys[[nv]] <- rast(filename)
  } else {
    drys[[nv]] <- dryspellapprast(precipitation_dataset_daily,valmin=valmin,months=months,filename=filename,overwrite=TRUE)
  }
  filename_aggr <- drys_grd_aggr[nv]
  cond <- file.exists(filename_aggr)
##  cond <- FALSE
  if (cond) {
    drys_aggr[[nv]] <- rast(filename_aggr)
  } else {
    drys_aggr[[nv]] <- aggregate(drys[[nv]],filename=filename_aggr,fact=10,fun="max",overwrite=TRUE)
   ## drys_aggr[[nv]] <- resample(drys[[nv]],y=out[[1]],filename=filename_aggr,method="max",overwrite=TRUE)
    
   
  }
 ## drys_aggr[[nv]] <- align(drys_aggr[[nv]],out[[1]])
  df <- as.data.frame(drys_aggr[[nv]],xy=TRUE) %>% melt(id=c("x","y"))
  names(df)[names(df)=="variable"] <- "year"
  names(df)[names(df)=="value"] <- "drys_value"
  df$drys_variable <- nv
  drys_aggr_df[[nv]] <- df  
  
}
drys_aggr_df2 <- rbindlist(drys_aggr_df)


out2 <- list()
out3 <- list()
for (crop in crops) {
  
  
  out2[[crop]] <- resample(out[[crop]],drys_aggr[[1]])
  df <- as.data.frame(out2[[crop]],xy=TRUE) %>% melt(id=c("x","y"))
  names(df)[names(df)=="variable"] <- "year"
 ## names(df)[length(names(df))+1] <- "variable"
  if (nrow(df)>0) {
    df$variable <- crop
  } else {
    
    df$variable <- character(0)
    
  }
  out3[[crop]] <- as.data.table(df)
}


out4 <- rbindlist(out3) 


out5 <- out4 %>% full_join(drys_aggr_df2) %>% filter(!is.na(variable)) %>% mutate(id=sprintf("x%f_y%f",x,y))
##out5 <- out4
####################
####################
####################
#
# ANALISYS
library(ggplot2)

out6 <- out5 %>% filter(!is.na(drys_value)) ### ,variable %in% c("maize","rice"))


#gg <- ggplot(+geom_point(aes(x=drys_value,y=value,color=variable,group=variable),data=out6)+theme_bw()

breaks=c(0,2,5,10,15,20,30)
breaks=c(0,15,25,Inf)

out7 <- out6 %>% mutate(drys_class=cut(drys_value,breaks))
### https://ggplot2.tidyverse.org/reference/geom_boxplot.html

#gg <- ggplot()+geom_boxplot(aes(x=drys_class,y=value,color=variable,group=drys_class),data=out7)+geom_jitter(width = 0.2)+theme_bw()

gg <- ggplot(aes(x=drys_class,y=value),data=out7)
gg <- gg+geom_boxplot(aes(colour=variable))
gg <- gg+facet_wrap(. ~ drys_variable)
gg <- gg+theme_bw()


























