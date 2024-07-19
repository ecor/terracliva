NULL
#' 
#' Computes a function on multi-layer raster, specially thought for Climate Variability Analysis in Spatial Gridded Coverage
#' 
#'
#' @param x a \code{SpatRast-Class} object
#' @param fun function. Default is \code{\link{samlmu}}. See \code{\link{app}},\code{\link{tapp}}
#' @param index see \code{\link{app}}. It can be set equal to \code{"monthly"}
#' 
#' @param na.rm,... further arguments for \code{fun}, \code{\link{app}} and \code{\link{tapp}}
#' 
#' @importFrom magrittr  %>% 
#' @importFrom terra app nlyr tapp time
#' @importFrom lmom samlmu
#' @importFrom lubridate month 
#' 
#' @export
#'
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
#' out_yearly <- apprast(dataset_yearly)
#' ## Function must contain a na.rm or ... argument. 
#' funpel <- function(x,distrib="pe3",...) {
#'   o1 <- samlmu(x) 
#'   o <- o1 %>% pel(distrib=distrib)
#'   nn <- names(o)
#'   o <- as.numeric(o)
#'   names(o) <- paste(distrib,nn,sep="_")
#'   o <- c(o1,o)
#'   return(o)
#' }
#' 
#' out_yearly_pel <- apprast(dataset_yearly,fun=funpel)
#' 
#' library(lubridate)
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' time(dataset_monthly) <-  names(dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#' index_monthly  <- month(time(dataset_monthly)) %>% sprintf(fmt="M%02d")
#' 
#' out_monthly <- apprast(dataset_monthly,index=index_monthly)
#' out_monthly_pel <- apprast(dataset_monthly,fun=funpel)
#' 
#' 
#' 
#' 
#' 
#'
apprast <- function(x,index=1,fun=samlmu,na.rm=TRUE,...){
  
  ##out <- tapp(x,indexx,fun=samlmu)
  if (length(index)<1) index <- 1 
  if (("na.rm" %in% names(formals(fun)))) {
    na.rm.exists=TRUE
  } else {
    na.rm.exists=FALSE
  } 
  
  
  if (index[1]=="monthly") index <- month(time(x)) %>% sprintf(fmt="M%02d")
  
  if (length(index)==nlyr(x) & nlyr(x)>1) {
    
    if (na.rm.exists) {
      out <- tapp(x,index=index,fun=fun,na.rm=na.rm,...)
    } else {
      out <- tapp(x,index=index,fun=fun,...)
    }
  } else  if (na.rm.exists) {
    
    out <- app(x,fun=fun,na.rm=na.rm,...)
    
  } else {
    
    out <- app(x,fun=fun,...)
  }
  
  return(out)
  
}
# @warning \code{x} must have the proper time aggregation for the analysis before the execution of this function.
