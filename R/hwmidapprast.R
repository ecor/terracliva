NULL
#'  Heat and Cold waves analysis in Spatial Gridded Coverage
#'
#'  Heat and Cold waves analysis in Spatial Gridded Coverage
#' 
#' @param x time series a \code{SpatRast-Class} object (e.g. daily maximum or minimum  temperature) 
#' @param timex corresponding vector of dates for \code{x}
#' @param index,fun further arguments passed to \code{\link{apprast}}
#' @param ... further arguments for \code{\link{hwmidcliva}} or \code{\link{cwmidcliva}}, \code{\link{apprast}}(then also \code{\link{writeRaster}})
#'
#'
#' @export
#'
#' @seealso \code{\link{hwmidcliva}},\code{\link{cwmidcliva}},\code{\link{hwmid}}
#' @examples
#' 
#' library(magrittr)
#' library(terra)
#' library(lmomPi)
#' library(extRemes)
#' years <- 1983:2016
#' tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
#' tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% 
#' sprintf(tmax_dataset_path,years) %>% rast()
#'
#' 
#'
#' o_hw <- hwmidapprast(tmax_dataset_daily)
#' o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE)
#'
#' ## COLD WAVE 
#' tmin_dataset_path <- system.file("ext_data/tmin",package="terracliva")
#' tmin_dataset_daily <- "%s/daily/chirts_daily_goma_tmin_%04d.grd" %>% 
#' sprintf(tmin_dataset_path,years) %>% rast()
#'
#' o_cw <- cwmidapprast(tmin_dataset_daily)
#' 
#' 
hwmidapprast <- function(x,timex=time(x),fun=hwmidcliva,index=1,...){
  
 
  out <- apprast(x,timex=timex,return_vector=TRUE,fun=fun,index=index,...)
  
  return(out)
  
}


NULL
#'
#' @name hwmidapprast
#' @export
#' 
#' @rdname hwmidapprast
#' @aliases cwmidapprast
#' 
#' 
cwmidapprast <- function(x,timex=time(x),fun=cwmidcliva,index=1,...) {
  
  out <- hwmidapprast(x,timex=timex,fun=cwmidcliva,index=index,...)
  return(out)
}

