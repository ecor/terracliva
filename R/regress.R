NULL
#' Correlation and Mann-Kendall correlation test 
#'
#' Correlation and Mann-Kendall correlation test 
#'
#' @param x time series (vector)
#' @param time date-time vector, if \code{NULL} , time is the index of \code{x}
#' @param signif test significance
#' @param frequency integer specifying the frequency of the time series \code{x} (e.g., 12 for monthly data, 365 for daily data). When \code{x} is monthly (frequency = 12), the function performs separate regressions for each month.
#' @param formatter character used in case of \code{frequency>0} 
#' @param na.rm logical or numeric evaluating to \code{TRUE} or \code{FALSE} or something else indicating whether or how many NA values should be stripped before the computation proceeds. Details in function code. 
#' @export
#'
#'
#' @importFrom stats lm quantile
#' @importFrom trend sens.slope
#'
#'
#' @examples 
#' 
#' library(lubridate)
#' set.seed(77)
#' start <- as.Date("1990-01-01")
#' time <- start+lubridate::years(1:30)
#' print(time)
#' x <- rnorm(length(time))
#' out <- regress(x=x,time=time)
#' 
#' start1 <- start
#' end1 <- start1+years(30)
#' timex1 <- seq(from=start1,to=end1,by="day")
#' x1 <- rnorm(length(timex1))
#' out1 <- regress(x=x1,time=timex1,frequency=12)
#'


regress <- function(x,time=NULL,signif=0.1,na.rm=0.3,frequency=NA,formatter="M%02d") {  
  if (is.null(frequency)) frequency <- NA
  if (!is.na(frequency) & (frequency>1)) {
    out <- list()
    mm <- rep(1:frequency,length.out=length(x))
    for (m in 1:frequency) {
      xm <- x[mm==m]
      timem <- time
      if (length(timem)>0) timem <- time[mm==m]
      out[[sprintf(formatter,m)]] <- regress(xm,timem,signif=signif,na.rm=na.rm,frequency=NA)
      
      
      
      
    }
    
    return(out)
  }
  
  
  if (is.numeric(na.rm)) na.rm <- length(is.na(x))<=na.rm*length(x)
  
  if (na.rm) x <- x[!is.na(x)]
  if (is.null(time)) time=(1:length(x))-1
  
  
  time=(1:length(x))-1
  
  
  
  if (all(is.na(x))) {
    x <- array(-0.0001,length(x))
    condNA <- TRUE
    condNA1 <- TRUE
  } else if(any(is.na(x))){
    condNA <- FALSE
    condNA1 <- TRUE
  } else {  
    condNA <- FALSE
    condNA1 <- FALSE
  }	
  
  out <- (lm(x ~ time))
  
  ## Analysis 
  out <- summary(out)
  
  ii <- which(rownames(out$coefficient)=="time")
  if (length(ii)>0) {
    ii <- ii[1]
    pvalue <- out$coefficient[ii,"Pr(>|t|)"]
    coeff  <- out$coefficient[ii,"Estimate"]
    stderror <- out$coefficient[ii,"Std. Error"]
    
  } else {
    
    pvalue <- as.numeric(NA)
    coeff <- as.numeric(NA)
    stderror <- as.numeric(NA)
    
  }
  rsquared <- out$r.squared
  
  coeff[pvalue>signif | condNA] <- NA
  stderror[pvalue>signif | condNA] <- NA
  rsquared[pvalue>signif | condNA] <- NA
  pvalue[condNA] <- NA
  out <- c(pvalue=pvalue,coeff=coeff,stdrerror=stderror,rsquared=rsquared)
  
  ### Mann-Kendall Test
  if (condNA1) {
    
    out <- c(out,senslope=NA,pvalue_mk=NA)
    
  } else { 
    
    sens <- sens.slope(x)
    senslope=sens$estimate
    pvalue_mk <- sens$p.value
    senslope[pvalue_mk>signif | condNA] <- NA
    pvalue_mk[condNA] <- NA
    out <- c(out,senslope=senslope,pvalue_mk=pvalue_mk)
    
  }
  
  names(out) <- c("pvalue","coeff","stdrerror","rsquared","senslope","pvalue_mk")
  
  return(out)
}
NULL
#'Useful Aggregation Functions
#'
#'Useful Aggregation Functions
#'
#'@param x time series or vector
#'@param na.rm logical. Remove or not NA values. See \code{\link{max}}
#'@param mn,mx nimumum (closed / included)  and maximum (open / excluded) thresholds respectively.
#'@param aggr_fun_suffixes Aggregation functions (e.g. used by \code{\link{dryspellcliva}}) See function usage for default.
#'
#'
#'
#' @export
#' 
#' 
q25 <- function(x,na.rm=FALSE) {quantile(x,0.25,na.rm=na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
q75 <- function(x,na.rm=FALSE) {quantile(x,0.75,na.rm=na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
q90 <- function(x,na.rm=FALSE) {quantile(x,0.90,na.rm=na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
q90_7 <- function(x,na.rm=FALSE) {quantile(x[x>=7],0.90,na.rm=na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
iqr <- function(x,na.rm=FALSE) {q75(x,na.rm)-q25(x,na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
drySpellCount <- function(x,na.rm=TRUE) {length(x)}
NULL
#' @export
#' @rdname q25
#' 
#' 
drySpellCount_mm <- function(x,mn=3,mx=10,na.rm=TRUE) {length(x[which((x>=mn) & (x<mx))])}
NULL
#' @export
#' @rdname q25
#' 
#' 
drySpellCount_003_010_days <- function(x,na.rm=TRUE) {drySpellCount_mm(x,mn=3,mx=10,na.rm=na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
drySpellCount_010_021_days <- function(x,na.rm=TRUE) {drySpellCount_mm(x,mn=10,mx=21,na.rm=na.rm)}
NULL
#' @export
#' @rdname q25
#' 
#' 
drySpellCount_021_999_days <- function(x,na.rm=TRUE) {drySpellCount_mm(x,mn=21,mx=Inf,na.rm=na.rm)}

NULL
#' @export
#' @rdname q25
#' 
#' 
aggr_fun_suffixes <- function(aggr_fun_suffixes=c("drySpellCount_003_010_days","drySpellCount_010_021_days","drySpellCount_021_999_days","drySpellCount","median","max","q25","q75","mean","q90","q90_7","iqr","sum")){return(aggr_fun_suffixes)}