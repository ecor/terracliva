NULL
#'
#' Dry Spell Analysis
#'
#' @param x time series (e.g. daily precipitation)
#' @param timex corresponding vector of dates for \code{x}
#' @param valmin minimum value for rainfall  
#' @param months months of the rainy season  
#' @param start_day starting day of the month. It can be different from 1.  
#' @param fun_aggr character aggregation function name. 
#' @param thres_value threshold value as a maximum length in days for a dry spell. 
#' @param set_thres_value_as_na logical it sets \code{thres_value} (if possible) when the value is previously not assigned
#' @param ... further arguments
#'
#'
#'
#' @importFrom stringr str_split
#'
#' @export
#'
#'
#'
#'
#' @examples
#'
#'
#'
#'
#' library(magrittr)
#' library(terra)
#'
#'
#' years <- 1982:2023
#' dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' 
#' 
#' prec <- as.numeric(dataset_daily[100])
#' timeprec <- time(dataset_daily)
#' 
#' out <- dryspellcliva(prec,timeprec)
#' out_df <- dryspellcliva(prec,timeprec,fun_aggr=NULL)
#' out10 <- dryspellcliva(prec,timeprec,valmin=10)
#' 
#' precb <- prec
#' precb[1:500] <- 10
#' outb_max <- dryspellcliva(precb,timeprec,valmin=1,fun_aggr="max")
#' outb_mean <- dryspellcliva(precb,timeprec,valmin=1,fun_aggr="mean")
#' outb_several <- dryspellcliva(precb,timeprec,valmin=1,fun_aggr=c("q25","median","mean","q75","q90","max"))
#' 
#' 
#' 
#' 
#' @references 
#' 
#' Usman, M.T. and Reason, C.J.C. (2004) Dry Spell Frequencies and Their Variability over Southern Africa. Climate Research, 26, 199-211.
#' \url{https://doi.org/10.3354/cr026199}
#' 
#' Thoithi, W., Blamey, R. C., & Reason, C. J. C. (2021). 
#' Dry spells, wet days, and their trends across southern Africa during the summer rainy season. 
#' Geophysical Research Letters, 48, e2020GL091041. 
#' \url{https://doi.org/10.1029/2020GL091041}
#' 
#' 
#' 
#' 
###

dryspellcliva <- function(x,timex,valmin=1,months=c(12,1,2,3),start_day=1,fun_aggr="max",thres_value=150,set_thres_value_as_na=FALSE,...) {
  
  
  

 


  x2 <- x>=valmin
 ## CHECK OTHER CONDITIONS???
  x3 <- cumsum(x2)

  drys <- split(x2,f=x3)
  drys_time <- split(timex,f=x3)
  drys_xval <- split(x,f=x3)
  cond_ok <- all(sapply(drys,FUN=function(r){r[1]}))
  if (!cond_ok) cond_ok <- which(!sapply(drys,FUN=function(r){r[1]}))==1
  xval99 <<- x
  out_drys99 <<- drys
  out_drys99_time <<- drys_time
  out_drys99_xval <<- drys_xval
  if (!cond_ok) {
   
    stop("ERROR in dry spell detection")
  }
  
  lens <- as.numeric(sapply(X=drys,FUN=function(r){length(r[-1])}))
  lens[lens>thres_value] <- thres_value
  ####
  ####
  ####
  vals <- as.numeric(sapply(X=drys_xval,FUN=function(r){sum(r[-1])})) ## rainfall amount
  ilens <- which(lens>0)
    
    #drys1 <- lapply(X=drys,FUN=function(r){r[-1]})
  #drys1_val <- lapply(X=drys_val,FUN=function(r){sum(r[-1])})
  #out <- data.table()
  #ll <- sapply(drys1,FUN=length)
  #iccll <- which(ll>0)
 
  out <- data.frame(spell_length=as.numeric(lens[ilens])) ### PUT THRESHOLD HERE!!!
  
  out$spell_amount <- as.numeric(vals[ilens])
  out$monthxn <- as.numeric(sapply(X=drys_time[ilens],FUN=function(r,d=start_day){terracliva::monthx(r[1],start_day=d)})) 
  out$yearx <- as.numeric(sapply(X=drys_time[ilens],FUN=function(r,m=months[1],d=start_day){yearx(r[1],start_month=m,start_day=d)})) #%>% as.Date()
  out001 <<- out
  
  out <- out[which(out$monthxn %in% months),]
  
   ####out$spell_end_date <- lapply(X=drys_time,FUN=function(r){format(r[length(r)])}) #%>% as.Date()
  ###if (is.na(fun_aggr)) fun_aggr <- NULL
  out002 <<- out
  if (!is.null(fun_aggr))  {
    
    ###
    ##fun_aggrs <- fun_aggr
    
    outf <- list()
    year_u <- sort(unique(yearx(timex,start_day=start_day,start_month=months[1])))  ##min(out$yearx):max(out$yearx)
    out3 <- array(as.numeric(NA),length(year_u))
    names(out3) <- year_u
    for (itf in fun_aggr) {
      
      out2 <- tapply(out$spell_length,FUN=get(itf),INDEX=out$yearx,simplify=TRUE,...)
      out3a <- out3
      out3a[names(out2)] <- out2[names(out2)]
      out3aa <<- out3a
      out2aa <<- out2
      out3a <- out2[as.character(sort(as.numeric(names(out3a))))]
      out3a[set_thres_value_as_na & is.na(out3a)] <-  thres_value
      names(out3a) <- paste(itf,names(out3a),sep="_")
      outf[[itf]] <- out3a
      
    } 
    
    ##
    
   
    
    out <- unlist(outf)
    names(out) <- sapply(str_split(names(out),"[.]"),FUN=function(x){x[[2]]})
     ## if (is.character(itf)) itf <- get(itf)
    
    #####
    
    # fun_aggr2 <- function(x,...) {
    #   print(x)
    #   if (length(x)<1) {
    #     
    #       o <- -1        
    #     
    #   } else {
    #     
    #     o <- fun_aggr(x,...)
    #   }
    #   return(o)
    # }
    # 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 
    # 23   22   29   26   21   36   19   13   30   22   18   45   22   29   13   27   51   26   26   19   17   18   17   25   27 
    # 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 
    # 25   19   18   40   21   26   45   26   25   38   32   31   28   19   46    6 
    
    
    #   out2 <- tapply(out$spell_length,FUN=fun_aggr,INDEX=out$yearx,simplify=TRUE,...)
    # #####
    #   year_u <- sort(unique(yearx(timex,start_day=start_day,start_month=months[1])))  ##min(out$yearx):max(out$yearx)
    #   out3 <- array(as.numeric(NA),length(year_u))
    #   names(out3) <- year_u
    #   out3[names(out2)] <- out2[names(out2)]
    # #out2[as.character(year_u[which(!(as.character(year_u) %in% names(out2)))])] <- as.numeric(NA)
    #   out2 <- out3                
    #   out2 <- out2[as.character(sort(as.numeric(names(out2))))]
    # ###
    # 
    # ###
    #   out2[set_thres_value_as_na & is.na(out2)] <-  thres_value
    #   out[[]] <- out2
    
    out_cliva <<- out
    ###print(out)
  }
  
  #out <- as.data.frame(out)

  return(out)



}
