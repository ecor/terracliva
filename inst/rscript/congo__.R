###
###


vect_f <-  "/home/ecor/local/data/spatial/sadec/Shapefiles//DemocraticRepublicOfCongo.shp"  %>% st_read()
prec_congo <- crop(prec,vect_f)
fgh
