NULL
#' 
#' SPI / SPEI index  (see lmomPi implementation)
#' 
#' @param x time series (e.g. monthly precipitation)
#' @param timex corresponding vector of dates for \code{x}. It is a vector of dates of each first day of the month
#' @param timex_ref corresponding vector of dates in which probability distribution (with its parameters) is estimated
#' @param distrib probability distribution function. See \code{\link{pel}}
#' @param spi.scale integer value or \code{NA}. If it greater than 1 \code{x} is filtered with the sum of a generic element of \code{x} and the previous \code{spi.scale-1} ones (e.g. SPI-3,SPI-6, etc. ). Default is \code{NA} (no filtering) which is equivalent to \code{spi.scale=1}.
#' @param index argument it is set equal to \code{"monthly"}
#' @param na.rm logical or numeric evaluating to \code{TRUE} or \code{FALSE} or something else indicating whether or how many NA values should be stripped before the computation proceeds. Details in function code. 
#' @param summary_regress logical value. Default is \code{FALSE} , if \code{TRUE} summary with \code{\link{regress}} is shown.
#' @param signif test significance, see \code{\link{regress}}.
#' @param pthres tail probability thresholds , in case of regression absolute values greater than \code{-qnorm(pthres)} are cut off. 
#' @param ... further arguments
#'
#' @param ... further arguments
#'
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom lmomPi spi.cdf
#' @importFrom stats qnorm
#' 
#' 
#' @seealso \code{\link{spi.cdf}}
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
#' 
#' 
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% 
#' sprintf(dataset_path,years) %>% rast()
#' terra::time(dataset_monthly) <-  names(dataset_monthly) %>% 
#' paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#'
#' prec <- as.numeric(dataset_monthly[100])
#' timeprec <- terra::time(dataset_monthly)
#' spi1 <- spicliva(x=prec,timex=timeprec,distrib="pe3")
#' spi1 <- spicliva(x=prec,timex=timeprec)
#' spi3 <- spicliva(x=prec,timex=timeprec,spi.scale=3)
#'  
#' spi1r <- spicliva(x=prec,timex=timeprec,summary_regress=TRUE)
#' spi3r <- spicliva(x=prec,timex=timeprec,spi.scale=3,summary_regress=TRUE)
#' 
#' 
#' 
#' 
  
spicliva <- function(x,timex,timex_ref=timex,distrib="pe3",spi.scale=1,index="monthly_spi",summary_regress=FALSE,pthres=10^-5,signif=0.1,na.rm=0.3,...) {
  
  
    if (length(x)!=length(timex)) {
    
      msg <- sprintf("Mismatch between x (%d) and timex (%d)!",length(x),length(timex))
      stop(msg)
     }
     if (index=="monthly_spi") {
       indices <- sprintf("M%02d",lubridate::month(timex))
       
     } else {
       indices <- array("M00",length(timex))
       warning("Index is not monthly, this option is not well implemented.")
     } 
  ###   indices <- sprintf("M%02d",month(timex))
  
     iref <- which(timex %in% timex_ref)
     para  <- pel(x=x[iref],indices=indices[iref],distrib=distrib,
                           spi.scale=spi.scale)
     o <- spi.cdf(x=x,indices=indices,para=para,spi.scale=spi.scale)
     ###o <- NULL
     
     
     
     
     
     
     names(o) <- timex
     
     if (summary_regress) {
       thres <- -qnorm(pthres)
       o[o<(-thres)] <- -thres
       o[o>thres]    <- thres
       
       o2 <- terracliva::regress(x=o,time=timex,signif=signif,na.rm=na.rm)
       names(o2) <- "spi_%s" |> sprintf(names(o2))
       o <- c(o,o2)
     
     }
     return(o)
}
  
 