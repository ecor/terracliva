rm(list=ls())


library(sf)
library(stringr)

## GOMA AREA
## https://data.humdata.org/dataset/01a0f7b2-5c59-46b1-b7cb-887058e44536/resource/450d8af6-c868-426d-aefc-81c4bd011243/download/osm_goma_quartiers_210527.zip
## Download 2024 01 03
dsn <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/OSM_Goma_quartiers_210527.shp"
uu <- st_read(dsn)
###

###












