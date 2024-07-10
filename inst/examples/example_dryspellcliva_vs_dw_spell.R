library(magrittr)
library(terra)
library(terracliva)
library(RGENERATEPREC)
library(lubridate)

##source("/home/ecor/local/rpackages/jrc/terracliva/R/dryspellcliva.R")

years <- 1982:2023



dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()


prec <- as.numeric(dataset_daily[100])
timeprec <- time(dataset_daily)
min_dry_spell_length = 3
months = c(12, 1, 2, 3)
####


#####
cc1 <- system.time(out_df <- dryspellcliva(prec,timeprec,fun_aggr=NULL,min_dry_spell_length = min_dry_spell_length,months=months,valmin=1,dryspell_ends_in_months=TRUE))
cc2 <- system.time({out_df_dwspell <- dw.spell(prec,origin=timeprec[1],extract="dry",month=months,valmin=1,from.start=TRUE)[[1]]

out_df_dwspell <- out_df_dwspell[which(out_df_dwspell$spell_length>=min_dry_spell_length),]

})
kk <- which((out_df$start_date %in% out_df_dwspell$start_date))

out_df <- out_df[kk,]

out_df$start_date==out_df_dwspell$start_date

out_df$spell_length==out_df_dwspell$spell_length

