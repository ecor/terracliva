NULL
#' 
#' Precipitation Deficit with L--Moments  Climate Variability Analysis 
#' 
#' @param x time series (e.g. precipitation)
#' @param timex corresponding vector of dates for \code{x}. It is a vector of dates of each first day of the month
#' @param distrib probability distribution function. See \code{\link{pel}}
#' @param rt return periods for deficit and excess
#' @param pcts variation percentages: default os \code{NULL}, otherwise can be like \code{pcts=c(5,10,15,20,50)}
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} or something else indicating whether or how many NA values should be stripped before the computation proceeds. Details in function code. 
#' @param nmom see \code{\link[lmom]{samlmu}}
#' @param add_t_2,add_t,add_l_cv logical, if one of them is \code{TRUE}, \code{L-CV} or \code{t,t_2} ratio is calculated.
#' @param summary_regress logical value. Default is \code{FALSE} , if \code{TRUE} summary with \code{\link{regress}} is shown.
#' @param signif test significance, see \code{\link{regress}}.
#' @param check_lmoms logical. If it is \code{TRUE}, L-moments are checked through \code{\link{are.lmoms.valid}} 
#' @param ... further arguments
#'
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom lmomPi pel qua cdf are.lmoms.valid
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
#' out_yearly <- lmcliva(prec,distrib="pe3",pcts=c(5,10,15,20,50))
#' ks.test(x=prec,y=cdf,distrib="pe3",para=out_yearly[c(5,6,7)])
#'
#'

  
lmcliva <- function(x,timex=1:length(x),distrib="pe3",rt=c(2,5,10,20,50),pcts=NULL,na.rm=FALSE,summary_regress=FALSE,signif=0.1,add_t_2=FALSE,add_t=FALSE,add_l_cv=FALSE,nmom=4,check_lmoms=TRUE,...) {
  
    
    
     if (is.null(x) | all(is.na(x)) | (any(is.na(x)) & !na.rm)) {
       x[] <- -999
       cond_null <- TRUE
     } else{
       cond_null <- FALSE
     }
     o1 <- samlmu(x,nmom=nmom) 
     ####
     if (check_lmoms) {
       
       if (!are.lmoms.valid(o1)) o1[] <- as.numeric(NA)
       
     }
     
     
     ## added EC 20200304
     if (add_t) {
       o1[["t"]] <- o1[["l_2"]]/o1[["l_1"]]
     } else if (add_t_2) {
       o1[["t_2"]] <- o1[["l_2"]]/o1[["l_1"]]
     } else if (add_l_cv) {
       o1[["l_cv"]] <- o1[["l_2"]]/o1[["l_1"]]
       
     }
     # if (summary_regress) {
     #   
     #   o1a <- terracliva::regress(x=x,time=timex,signif=signif)
     #   
     #   o1 <- c(o1a,o1)
     # }
     
     
     if (length(distrib)==0) distrib <- NA ## 20250303
     if (!is.na(distrib)) {
     
      o2 <- pel(lmom=o1,distrib=distrib)
      ###
      ## CHECK pelpe3: L-moments invalid
      ## 
      ##
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
      ##
      ##qua(x=xq,distrib=distrib,para=o2[mm])
      if (is.null(pcts)) pcts <- NA 
      if(!is.na(pcts[1])) {
      mm <- o1[["l_1"]]
      v_pcts_exc <-mm*(1+pcts/100)
      names(v_pcts_exc) <- sprintf("prob_exc_%03d",pcts)
      
      v_pcts_def <- mm*(1-pcts/100)
      names(v_pcts_def) <- sprintf("prob_def_%03d",pcts)
      
      
      #ortdef <- cdf(x=)
      oprobexc <- cdf(x=v_pcts_exc,para=o2[nn],distrib=distrib)
      names(oprobexc) <- names(v_pcts_exc)
      oprobdef <- cdf(x=v_pcts_def,para=o2[nn],distrib=distrib)
      names(oprobdef) <- names(v_pcts_def) 
      ###
      ortexc <- 1/(1 - oprobexc) ## return period of an excess equal or greater than a value
      ortdef <- 1/oprobdef ## return period of a deficit equal or greater than a value
      names(ortexc) <- sprintf("rt_of_exc_%03d",pcts)
      names(ortdef) <- sprintf("rt_of_def_%03d",pcts)
      } else {
        oprobexc <- NULL
        oprobdef <- NULL
        ortexc <- NULL
        ortdef <- NULL
      }
      
      ##
      ##
      o2 <- c(o2,odefs,oexcs,oprobexc,oprobdef,ortexc,ortdef)
      names(o2) <- paste(distrib,names(o2),sep="_")
      o <- c(o1,o2)
      
     } else {
       o <- o1 
     }
   
     ### regress
     if (summary_regress) {
       
       o1a <- terracliva::regress(x=x,time=timex,signif=signif)
       
       o <- c(o,o1a)
     }
     o[cond_null] <- NA 
     
     
     
     return(o)
}
  
 