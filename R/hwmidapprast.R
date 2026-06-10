NULL
#'  Heat and Cold waves analysis in Spatial Gridded Coverage
#'
#'  Heat and Cold waves analysis in Spatial Gridded Coverage
#' 
#' @param x time series a \code{SpatRast-Class} object (e.g. daily maximum or minimum  temperature) 
#' @param timex corresponding vector of dates for \code{x}
#' @param index,fun further arguments passed to \code{\link{apprast}}
#' @param start_month,... further arguments for \code{\link{hwmidcliva}} or \code{\link{cwmidcliva}}, \code{\link{apprast}}(then also \code{\link[terra]{writeRaster}})
#'
#' @importFrom terra project
#' @export
#'
#' @seealso \code{\link{hwmidcliva}},\code{\link{cwmidcliva}},\code{\link[extRemes]{hwmid}}
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
#' \donttest{
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
#' }
hwmidapprast <- function(x,timex=time(x),fun=hwmidcliva,index=1,start_month="default",...){
  if (start_month=="default") {
    ee <- x[[1]] |> terra::project("epsg:4326") |> ext()
    northern_hemishere <- (ee$ymin+ee$ymax)/2>=0
    if (northern_hemishere) {
      start_month <- 1 
    } else {
      start_month <- 7 
    }
    
  }
  
  print("hwmidapprast")
  print("month")
  print(sprintf("M%02d",start_month))
  out <- apprast(x,timex=timex,return_vector=TRUE,fun=fun,index=index,start_month=start_month,...)
  
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

