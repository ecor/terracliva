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
#' @param dryspell_starts_in_months logical if \code{TRUE} dry spell starting on a date within the selected \code{months} are considered  
#' @param dryspell_ends_in_months  logical if \code{TRUE} dry spell ending on a date within the selected \code{months} are considered
#' @param min_dry_spell_length minimum length (number of days) of a dry spell in order to be taken in consideration. Default is 3.  
#' @param na.rm a logical evaluating to \code{TRUE} or \code{FALSE} or something else indicating whether or how many NA values should be stripped before the computation proceeds. Details in function code. 
#' @param summary_regress logical value. Default is \code{FALSE} , if \code{TRUE} summary with \code{\link{regress}} is shown.
#' @param signif test significance, see \code{\link{regress}}.
#' @param ... further arguments 
#'
#'
#'
#' @importFrom stringr str_split
#' @importFrom lubridate days
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
#' outb_several <- dryspellcliva(precb,timeprec,valmin=1,
#'             fun_aggr=c("q25","median","mean","q75","q90","max"))
#' outb_several_regress <- dryspellcliva(precb,timeprec,valmin=1,
#'         fun_aggr=c("q25","median","mean","q75","q90","max"),
#'         summary_regress=TRUE)
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

dryspellcliva <- function(x,timex,valmin=1,months=c(12,1,2,3),dryspell_starts_in_months=TRUE,dryspell_ends_in_months=FALSE,start_day=1,fun_aggr="max",thres_value=150,set_thres_value_as_na=FALSE,min_dry_spell_length=3,na.rm=FALSE,summary_regress=FALSE,signif=0.1,...) {
  
  
  

 

  ##20240719 x_global <<- x 
  
  ##20240719 timex_global <<- timex
  if (is.null(x) | all(is.na(x))) {
    x[] <- -999
    cond_null <- TRUE
  } else{
    cond_null <- FALSE
  }
  
  
  x2 <- x>valmin
  spell_state <- array("dry",length(x2))
  spell_length <- array(1,length(x2))
  spell_state[which(is.na(x))] <- "na"
  spell_state[which(x2)] <- "wet"
  spell_end <- array(FALSE,length(x))
  spell_end[1] <- TRUE
  spell_amount <- x 
  
  for (i in 2:length(x2)) {
    
    if (spell_state[i]==spell_state[i-1]) {
      spell_end[i] <- TRUE 
      spell_end[i-1] <- FALSE
      spell_length[i] <- spell_length[i]+spell_length[i-1]
      spell_amount[i] <- spell_amount[i]+spell_amount[i-1]
    } else {
      
      spell_end[i] <- TRUE 
      spell_length[i] <- 1 
      spell_amount[i] <- x[i]
    }
  
    
  }
  
  
  out <- data.frame(date=timex,spell_state=spell_state,spell_length=spell_length,spell_end=spell_end,spell_amount=spell_amount)
  out <- out[which(out$spell_end),]
  out <- out[which(out$spell_state=="dry"),]
  ## wet 
  
  # 
  # for (c in 1:ncol(as.data.frame(data[,ignore.date]))){
  #   
  #   val <- as.vector(as.data.frame(data[,ignore.date])[,c])
  #   
  #   spell_state <- array("dry",length(val))
  #   
  #   spell_length <- array(1,length(val))
  #   ###	spellstart <- array(FALSE,length(val))
  #   spell_end <- array(FALSE,length(val))
  #   spell_state[which(is.na(val))] <- "na"
  #   spell_state[which(val>valmin)] <- "wet"
  #   
  #   
  #   spell_end[1] <- TRUE
  #   
  #   for (i in 2:length(spell_state)) {
  #     
  #     if (spell_state[i]==spell_state[i-1]) {
  #       
  #       spell_end[i] <- TRUE 
  #       spell_end[i-1] <- FALSE
  #       spell_length[i] <- spell_length[i]+spell_length[i-1]
  #     } else{ 
  #       
  #       spell_end[i] <- TRUE 
  #       spell_length[i] <- 1 
  #       
  #     }
  #     
  #     
  #   }
  #   
  #   spell_length <- spell_length[which(spell_end)]
  #   spell_state <- spell_state[which(spell_end)]
  #   
  #   temp <- data[which(spell_end),!ignore.date]
  #   
  #   temp$spell_length <- spell_length
  #   temp$spell_state <- spell_state
  #   temp$end_date <- as.Date(paste(temp$year,temp$month,temp$day,sep="-"))
  #   temp$start_date <- temp$end_date-lubridate::days(temp$spell_length-1)
  #   
  # 
  # 
  
  
  
 ## CHECK OTHER CONDITIONS???
  #x3 <- cumsum(x2)

  #drys <- split(x2,f=x3)
  #drys_time <- split(timex,f=x3)
  #drys_xval <- split(x,f=x3)
  #cond_ok <- all(sapply(drys,FUN=function(r){r[1]}))
  #if (!cond_ok) cond_ok <- which(!sapply(drys,FUN=function(r){r[1]}))==1
  #xval99 <<- x
  #out_drys99 <<- drys
  #out_drys99_time <<- drys_time
  #out_drys99_xval <<- drys_xval
  #if (!cond_ok) {
   
  #  stop("ERROR in dry spell detection")
  #}
  #
  #lens <- as.numeric(sapply(X=drys,FUN=function(r){length(r[-1])}))
  #lens[lens>thres_value] <- thres_value
  ####
  ####
  ####
  #vals <- as.numeric(sapply(X=drys_xval,FUN=function(r){sum(r[-1])})) ## rainfall amount
  #ilens <- which(lens>0)
    
    #drys1 <- lapply(X=drys,FUN=function(r){r[-1]})
  #drys1_val <- lapply(X=drys_val,FUN=function(r){sum(r[-1])})
  #out <- data.table()
  #ll <- sapply(drys1,FUN=length)
  #iccll <- which(ll>0)
 
  #out <- data.frame(spell_length=as.numeric(lens[ilens])) ### PUT THRESHOLD HERE!!!
  
  ##out$spell_amount <- as.numeric(vals[ilens])
  out$start_date <- out$date-lubridate::days(out$spell_length-1)
  out$monthxn <- terracliva::monthx(out$start_date,start_day=start_day)  ##  as.numeric(sapply(X=out$start_date,FUN=function(r,d=start_day){terracliva::monthx(r,start_day=d)})) 
  out$monthxn2 <- terracliva::monthx(out$date,start_day=start_day) ## as.numeric(sapply(X=out$date,FUN=function(r,d=start_day){terracliva::monthx(r,start_day=d)})) 
  ### 
  out$yearx <- yearx(out$start_date,start_month=months[1],start_day=start_day) ##as.numeric(sapply(X=out$start_date,FUN=function(r,m=months[1],d=start_day){yearx(r,start_month=m,start_day=d)})) #%>% as.Date()
  ##
  ##20240719 out_global <<- out 
  out$month_cond_on_dry_spell <- FALSE
  if (dryspell_starts_in_months) out$month_cond_on_dry_spell <- out$month_cond_on_dry_spell | (out$monthxn %in% months)
  if (dryspell_ends_in_months) out$month_cond_on_dry_spell <- out$month_cond_on_dry_spell | (out$monthxn2 %in% months)  
 
  
  
  ##EC 20240709 out001 <<- out
  ###
  
  out <- out[which(out$month_cond_on_dry_spell),]
  out <- out[which(out$spell_length>=min_dry_spell_length),]
  
  if(nrow(out) == 0){
    out[1,] = NA
    cond_null = TRUE
  }
  
  out$spell_length[cond_null] <- NA 
   ####out$spell_end_date <- lapply(X=drys_time,FUN=function(r){format(r[length(r)])}) #%>% as.Date()
  ###if (is.na(fun_aggr)) fun_aggr <- NULL
  ##EC 20240709 out002 <<- out
  if (!is.null(fun_aggr))  {
    
    ###
    ##fun_aggrs <- fun_aggr
    
    outf <- list()
    year_u <- sort(unique(yearx(timex,start_day=start_day,start_month=months[1])))  ##min(out$yearx):max(out$yearx)
   #### year_u1 <- sort(unique(year(timex))
   ####  year_u <- sort(unique(year(timex)))
            
                           
                  
                            
                   
    ## correction here 
    ##if (year(timex[1])==(year_u[1]+1)) year_u <- year_u[-1] ## 20241211 
    
    
    ##
    
     out3 <- array(as.numeric(NA),length(year_u))
    names(out3) <- year_u
  ##  out33aa <<- out3
  ##  outslll <<- out
    
    for (itf in fun_aggr) {
      ####print(na.rm)
      na.rm_global <- na.rm
      out2 <- tapply(out$spell_length,FUN=get(itf),INDEX=out$yearx,simplify=TRUE,na.rm=na.rm,...)
      out3a <- out3
     ## names(out3a) <- paste(itf,names(out3),sep="_")
      out3a[names(out2)] <- out2[names(out2)]
      
      
      ##EC 20240709out3aa <<- out3a
      ##EC 20240709 out2aa <<- out2
      
      
      
      out3a <- out3a[as.character(sort(as.numeric(names(out3a))))]
      out3a[set_thres_value_as_na & is.na(out3a)] <-  thres_value
      
      ##out3aglo <<- out3a
      if (summary_regress) {
        
        o2 <- terracliva::regress(x=out3a,time=as.numeric(names(out3a)),signif=signif)
        
        out3a <- c(out3a,o2)
      }
      
      
      
      
     ## out3aglo2 <<- out3a
      
      names(out3a) <- paste(itf,names(out3a),sep="_")
      outf[[itf]] <- out3a
      ##out3aglo3 <<- out3a
    } 
    
    
    
    
    ##
    
    ## outfglo1 <<- outf
    
    out <- unlist(outf)
    
   ### outfglo2 <<- out
    names(out) <- sapply(str_split(names(out),"[.]"),FUN=function(x){x[[2]]})
    
    ###cond_null_glob <<- cond_null
    ###outglo0 <<- out
    
    out[cond_null] <- NA
    
   #### outglobal <<- out
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
    
    ##EC 20240709 out_cliva <<- out
    ###print(out)
    
  }
  
  #out <- as.data.frame(out)
  
  
                    
    
    
    
  #}
  
  
  
  
  return(out)



}
