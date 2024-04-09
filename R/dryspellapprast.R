NULL
#'
#' Dry Spell Analysis in Spatial Gridded Coverage
#'
#' @param x time series (e.g. daily precipitation)
#' @param timex corresponding vector of dates for \code{x}
#' @param index,fun further arguments passed to \code{\link{apprast}}
#' @param ... further arguments for \code{\link{dryspellcliva}} 
#'
#'
#'
#'
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
#' out <- dryspellapprast(dataset_daily,valmin=2)
#' ## out5 <- dryspellapprast(dataset_daily,valmin=5)
#' ## out9 <- dryspellapprast(dataset_daily,valmin=9)
#' 
#' 
#' dataset_daily_n <- dataset_daily
#' dataset_daily_n[100][1:500] <- 10
#' out <- dryspellapprast(dataset_daily_n,valmin=2)
#' 
#' 
#' ## out10 TO BE TESTED # put a  a thresholds!!!!
#' @seealso \code{\link{dryspellapprast}}

dryspellapprast <- function(x,timex=time(x),fun=dryspellcliva,index=1,...){
  
  
  out <- apprast(x,timex=timex,fun=fun,index=index,...)
  
  return(out)
  
}

