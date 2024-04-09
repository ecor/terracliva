rm(list=ls())


library(enexusClimate)
library(stringr)

dsn <- system.file('mekrou/Mekrou_shapefile/Mekrou_AOI_v3.shp',package="enexusClimate")


uu <- st_read(dsn)
names(uu) <- tolower(names(uu))
uu$name <- str_replace(uu$name,"é","e")
#"Kouandé"   "Péhunco"   "Kérou"     "Banikoara" "Diapaga"   "Tansarga"  "Bottou"    "Parc W"    "Tamou"     "Kirtachi"  "Falmey"   
#[12] "Karimama" 
dsn1 <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/mekrou.shp"
st_write(uu,dsn1)