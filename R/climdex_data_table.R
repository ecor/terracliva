NULL
#' ClimDExInput ... 
#' 
#' This function computes ...
#' This function takes a climdexInput object as input and computes the climdex
#' index Rx1day: monthly, seasonal or annual maximum 1-day precipitation.
#' 
#' @param  x a data frame or a data table
#' @param tmax,tmin,prec vector of daily maximum and minim temperature and precipiation (otherwise taken from \code{x})
#' @param timex vector of dates / time  (otherwise taken from \code{x})
#' @param ntmax,ntmin,ntavg,nprec  column names of \code{x} for daily maximum and minimum and avarage temperature and precipitation.
#' @param ntime column me of \code{x} for date / time coordinate
#' @param base.range,... further arguments for \code{\link[climdex.pcic]{climdexInput.raw}}
#' 
#' @importFrom zoo index 
#' @importFrom climdex.pcic   climdexInput.raw
#' @importFrom PCICt as.PCICt
#' 
#' 
#' @return a ClimDEx Input object, see  \code{\link[climdex.pcic]{climdexInput.raw}}
#' 
#' @seealso \code{\link[climdex.pcic]{climdexInput.raw}},\url{https://climate-scenarios.canada.ca/?page=climdex-indices}
#' 
#' @references Dosio, A., Mean and extreme climate in Europe under 1.5, 2, and 3 degC global warming, EUR 30194 EN, Publications Office of the European Union, Luxembourg, 2020, ISBN 978-92-76-18430-0, \doi{10.2760/826427}, JRC120574
#' @export
#' 
#' 
#' 
#' @examples
#' 
#' ## https://climate-scenarios.canada.ca/?page=climdex-indices
#' ## https://publications.jrc.ec.europa.eu/repository/handle/JRC120574
#' data(lusaka)
#' ci <- climdex_data_table(lusaka,nprec="precipitation",northern.hemisphere=FALSE)
#' 
#' r10mm <- climdex.r10mm(ci)
#' sdii <- climdex.sdii(ci)
#' 
#' df <- lusaka
#' 
#' 
#' 
climdex_data_table <- function(x,tmax=NULL,tmin=NULL,prec=NULL,timex=NULL,...,ntmax="tmax",ntmin="tmin",nprec="prec",ntavg="tavg",ntime="time",base.range=NULL) {
  
  timex <- NULL
  if (is.null(timex)) {
    timex <- try(index(x),silent=TRUE)
    if (inherits(timex,"try-error")) {
      timex <- try(index(x),silent=TRUE)
      if (inherits(timex,"try-error")) timex <- NULL
    
    } 
  }
  
  x <- as.data.frame(x)
  if (is.null(timex)) timex <- x[,ntime] 
  timex <- as.Date(timex)
 
  if (is.null(base.range)) base.range <- c(NA,NA)
  if (any(is.na(base.range))) base.range <- range(year(timex))
  timex_pcict <- as.PCICt(format(timex),format="%Y-%m-%d",cal="gregorian")
  
  if (is.null(tmax)) tmax <- x[,ntmax]
  if (is.null(tmin)) tmin <- x[,ntmin]
  if (is.null(prec)) prec <- x[,nprec]
  
  if (ntavg %in% names(x)) {
    tavg=x[,ntavg]
  } else { 
    tavg <- NULL
    }
  out <- climdexInput.raw(tmax=tmax,tmin=tmin,
                          prec=prec,tmax.dates=timex_pcict,
                          tmin.dates=timex_pcict,
                          prec.dates=timex_pcict,tavg=tavg,base.range = base.range,...)
  
  
  return(out)
}

