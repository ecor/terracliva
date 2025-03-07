NULL
#' 
#' Precipitation Deficit with L--Moments  Climate Variability Analysis 
#' 
#' @param x time series (e.g. precipitation)
#' @param timex corresponding vector of dates for \code{x}. It is a vector of dates of each first day of the month
#' @param distrib probability distribution function. See \code{\link{pel}}
#' @param rt return periods for deficit and excess
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} or something else indicating whether or how many NA values should be stripped before the computation proceeds. Details in function code. 
#' @param ... further arguments
#'
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom lmomPi pel qua cdf
#' @importFrom stats ks.test
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
#' dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' 
#' prec <- as.numeric(dataset_yearly[100])
#' 
#' out_yearly <- lmcliva(prec)
#' out_yearly <- lmcliva(prec,distrib="pe3")
#' ks.test(x=prec,y=cdf,distrib="pe3",para=out_yearly[c(5,6,7)])
#'
#'

  
lmcliva <- function(x,timex,distrib="pe3",rt=c(2,5,10,20,50),na.rm=FALSE,summary_regress=FALSE,signif=0.1,...) {
  
    
    
     if (is.null(x) | all(is.na(x)) | (any(is.na(x)) & !na.rm)) {
       x[] <- -999
       cond_null <- TRUE
     } else{
       cond_null <- FALSE
     }
     o1 <- samlmu(x) 
     ## added EC 20200304
     if (summary_regress) {
       
       o1a <- terracliva::regress(x=x,time=timex,signif=signif)
       
       o1 <- c(o1a,o1)
     }
     
     
     if (length(distrib)==0) distrib <- NA ## 20250303
     if (!is.na(distrib)) {
      o2 <- pel(lmom=o1,distrib=distrib)
      nn <- names(o2)
      o2 <- as.numeric(o2)
      names(o2) <- nn
    
     
      ### ks.value
      oks <- ks.test(x=x,y=cdf,distrib=distrib,para=o2[nn])
    
      o2["ks_D_statistic"] <- oks$statistic
      o2["ks_pvalue"] <- oks$p.value
     
     
      ### quantiles
      fdefs <- 1/rt
      fexcs <- 1-1/rt
      odefs <- qua(para=o2[nn],distrib=distrib,f=fdefs) ## precipitation that can be equal or lower every rt years averagely 
      names(odefs) <- sprintf("def_rt_%03d",rt) 
      odefsa <- odefs
      oexcs <- qua(para=o2[nn],distrib=distrib,f=fexcs) ## precipitation that can be equal or greater every rt years averagely
      names(oexcs) <- sprintf("exc_rt_%03d",rt)
      o2 <- c(o2,odefs,oexcs)
      names(o2) <- paste(distrib,names(o2),sep="_")
      o <- c(o1,o2)
      
     } else {
       o <- o1 
     }
     o[cond_null] <- NA 
     return(o)
}
  
 