NULL
#' 
#' SPI / SPEI index  (see lmomPi implementation)  Climate Variability Analysis in Spatial Gridded Coverage
#' 
#' @param x a \code{SpatRast-Class} object
#' @param timex corresponding vector of dates for \code{x}. It is a vector of dates of each first day of the month
#' @param distrib probability distribution function. See \code{\link{pel}}
#' @param fun argument passed to \code{\link{apprast}} Default is \code{\link{spicliva}}. See there further information.
#' @param index,na.rm,... further arguments passed to \code{\link{apprast}}(then also \code{\link{writeRaster}}) and  \code{fun} (\code{\link{lmcliva}} if \code{fun==lmcliva} (default). 
#' 
#'
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' 
#' @seealso \code{\link{spicliva}},\code{\link{spi.cdf}}
#' 
#' 
#' @examples
#' 
#' library(magrittr)
#' library(terra)
#' library(lmomPi)
#'
#' years <- 1982:2023
#' 
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% 
#' sprintf(dataset_path,years) %>% rast()
#' terra::time(dataset_monthly) <-  names(dataset_monthly) %>% 
#' paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#'
#'
#' spi1 <- spiapprast(x=dataset_monthly,distrib="pe3")
#' spi1 <- spiapprast(x=dataset_monthly)
#' spi3 <- spiapprast(x=dataset_monthly,spi.scale=3)
#' 
#' spi1r <- spiapprast(x=dataset_monthly,summary_regress=TRUE)
#' spi3r <- spiapprast(x=dataset_monthly,summary_regress=TRUE,spi.scale=3)
#'
  
spiapprast <- function(x,timex=time(x),index=1,distrib="pe3",fun=spicliva,na.rm=TRUE,...){
  
 #### function(x,timex,distrib="pe3",spi.scale=1,fun=spicliva,...) {
  
    
  out <- apprast(x,timex=timex,index=index,distrib=distrib,fun=fun,na.rm=na.rm,...)
  ##terra::time(out) <- timex
  
     return(out)
}
  
 