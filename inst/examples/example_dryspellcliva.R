library(magrittr)
library(terra)
library(lmomPi)

years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()


prec <- as.numeric(dataset_daily[100])
timex <- time(dataset_daily)

###
valmin <- 1


p2 <- prec>=valmin

p3 <- cumsum(p2)

drys <- split(p2,f=p3)
drys_time <- split(timex,f=p3)
cond_ok <- which(!sapply(drys,FUN=function(r){r[1]}))==1
if (!cond_ok) stop("ERROR")

drys1 <- lapply(X=drys,FUN=function(r){r[-1]})

out_l <- sapply(drys1,FUN=length)
out_date <- lapply(X=drys_time,FUN=function(r){r[1]})

###c(diff(p2),0)


##out_yearly <- lmcliva(prec)
##out_yearly <- lmcliva(prec,distrib="pe3")
##ks.test(x=prec,y=cdf,distrib="pe3",para=out_yearly[c(5,6,7)])