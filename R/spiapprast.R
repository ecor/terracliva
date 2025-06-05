NULL
#' 
#' SPI / SPEI index  (see lmomPi implementation)  Climate Variability Analysis in Spatial Gridded Coverage
#' 
#' @param x a \code{SpatRast-Class} object
#' @param timex corresponding vector of dates for \code{x}. It is a vector of dates of each first day of the month
#' @param distrib probability distribution function. See \code{\link{pel}}
#' @param fun argument passed to \code{\link{apprast}} Default is \code{\link{spicliva}}. See there further information.
#' @param spi.classes data frame with SPI/SPEI classes (see default csv file) 
#' @param add_cat logical, if \code{TRUE} SPI class categoriesare calculated for each month as attribute. 
#' @param index,na.rm,... further arguments passed to \code{\link{apprast}}(then also \code{\link{writeRaster}}) and  \code{fun} (\code{\link{lmcliva}} if \code{fun==lmcliva} (default). 
#' 
#'
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' 
#' @importFrom utils read.table
#' @importFrom stringr str_detect
#' @importFrom terra  classify coltab<- 
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
#' spi3r_cat <- spiapprast(x=dataset_monthly,spi.scale=3,add_cat=TRUE)
#'
#'
#'
  
spiapprast <- function(x,timex=time(x),index=1,distrib="pe3",fun=spicliva,na.rm=0.3,
                       add_cat=FALSE,
                       spi.classes=read.table(system.file("settings/spi_class.csv",package="terracliva"),header=TRUE,sep=",",comment.char="?"),...){
  
 #### function(x,timex,distrib="pe3",spi.scale=1,fun=spicliva,...) {
  
    
  out <- apprast(x,timex=timex,index=index,distrib=distrib,fun=fun,na.rm=na.rm,add_cat=FALSE,spi.classes=spi.classes,...)
  
  if (add_cat) {
    spi.classes$ID <- 1:nrow(spi.classes)
    
    rcl <- as.matrix(spi.classes[,c("min","max","ID")])
    outc <- classify(out[[str_detect(names(out),"_on_")]],rcl=rcl[,c(1,2,3)],include.lowest=TRUE)
    names(outc) <- names(out[[str_detect(names(out),"_on_")]])
    for (ii in 1:nlyr(outc)) {  
    
      coltab(outc,layer=ii) <- spi.classes[,c("ID","color")]
      levels(outc[[ii]]) <- spi.classes[,c("ID","name","color")]
    }
   
    attr(out,"spi_cat") <- outc
    attr(out,"spi_clasess")  <- spi.classes

  }

  ##terra::time(out) <- timex
  attr(out,"spi.scale") <- list(...)$spi.scale
  attr(out,"signif") <- list(...)$signif
  
     return(out)
}
  
 