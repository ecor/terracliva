library(magrittr)
library(terra)
library(data.table)
library(terracliva)
####
source("/home/ecor/local/rpackages/jrc/terracliva/R/dryspellcliva.R")
####


years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()

####
#### https://doi.pangaea.de/10.1594/PANGAEA.909132
####

prec <- as.numeric(dataset_daily[100])
timeprec <- time(dataset_daily)
valmins <- 1:20 ## 1:18 ##c(1,2,3,4,5,6,7,8,10,15) 
names(valmins) <- valmins %>% sprintf(fmt="P%02d")
out <- NULL
out <- list()

for (it in names(valmins)) {

  
  valmin <- valmins[it]
  out[[it]] <- dryspellcliva(prec,timeprec,valmin=valmin)

}

out <- as.data.frame(out)

out$year <- as.numeric(rownames(as.data.frame(out)))

out <- as.data.table(out)


## BOXPLOT 
library(ggplot2)

out2a <- melt(out,id="year")
out2 <- out2a
gg <- ggplot()+geom_boxplot(aes(x=variable,y=value,group=variable),data=out2)+theme_bw()


out18 <- dryspellcliva(prec,timeprec,valmin=18,fun_aggr=NULL)

