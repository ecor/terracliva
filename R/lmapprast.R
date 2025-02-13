NULL
#' 
#' Precipitation Defivit with L-Moments  Climate Variability Analysis in Spatial Gridded Coverage
#' 
#'
#' @param x a \code{SpatRast-Class} object
#' @param distrib probability distribution function. See \code{\link{pel}}
#' @param index,fun,na.rm,... further arguments passed to \code{\link{apprast}}(then also \code{\link{writeRaster}}) and  \code{fun} (\code{\link{lmcliva}} if \code{fun==lmcliva} (default). 
#' 
#' @importFrom magrittr  %>% 
#' @importFrom terra app nlyr tapp time
#' @importFrom lmom samlmu
#' @importFrom lubridate month 
#' 
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' 
#' 
#' @examples
#' 
#' library(magrittr)
#' library(terra)
#' library(lmomPi)
#' 
#' years <- 1982:2023
#' dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' 
#' 
#' 
#' out_yearly <- lmapprast(dataset_yearly)
#' 
#' 
#' library(lubridate)
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% 
#' sprintf(dataset_path,years) %>% rast()
#' time(dataset_monthly) <-  names(dataset_monthly) %>% 
#' paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#'
#' 
#' out_monthly <- lmapprast(dataset_monthly,index="monthly",distrib="pe3")
#' 
#' out_monthly_ <- lmapprast(dataset_monthly,index="monthly",distrib="pe3",mm=c(12,1,2,3))
#' 
#' 
#' 
#'
lmapprast <- function(x,index=1,distrib="pe3",fun=lmcliva,na.rm=TRUE,...){
  
  
  out <- apprast(x,index=index,distrib=distrib,fun=fun,na.rm=na.rm,...)
  
  return(out)
  
}