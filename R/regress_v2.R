NULL
#' Correlation and Mann-Kendall correlation test 
#'
#' Correlation and Mann-Kendall correlation test 
#'
#' @param x time series (vector)
#' @param time date-time vector
#' @param signif test significance
#' 
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
#' 
#'


regress <- function(x,time=time,signif=0.1) {  
  
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