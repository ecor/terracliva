NULL
#' 
#' p-Distance (p-norm of distance) between L-Moments of two different time series or samples derving from different datasets (e.g. preciptation datasets)(Spetially Gridded dataset)  
#' 
#' @param la,x  first set of L-moments  or L-moment ratios
#' @param lb  second set of L-moments  or L-moment ratios
#' @param fun argument passed to \code{\link{apprast}} Default is \code{\link{lmpdistcliva}}. See there further information.
#' @param na.rm,... further arguments for \code{fun}(e.g. \code{\link{lmpdistapprast}}) and \code{\link{apprast}}

#' @importFrom stringr str_detect str_sub str_replace str_length
#' 
#' 
#' @export
#'
#' @references  \url{https://en.wikipedia.org/wiki/Lp_space}
#' 
#' 
#' 
#' @examples
#' 
#' \donttest{
#' library(terra)
#' lmom_chirps <- system.file("ext_data/lm_chirps_2000_2024.tif",package="terracliva")  |> rast()
#' lmom_mswep  <- system.file("ext_data/lm_mswep_2000_2024.tif",package="terracliva")  |> rast()
#' 
#' out <- lmpdistapprast(la=lmom_chirps,lb=lmom_mswep,prefix="monthly")
#'
#' ##plot(Ou<0.#)
#' }
#' 
#' 


lmpdistapprast <- function(x,lb=NULL,fun=lmpdistcliva,la=x,na.rm=TRUE,...) {
  
  
  if (!is.null(lb)) {
    
    x <- list(la=la,lb=lb)
    
    prefix0 <- names(x)

    
  }
 
  out <- apprast(x,fun=fun,prefix0=prefix0,na.rm=na.rm,...)
  return(out)
  
  
}