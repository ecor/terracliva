NULL
#'
#' Month starting from a generic date
#'
#' @param x Date
#' @param start_day first day of the month of the considered Month
#' 
#' 
#'
#' @export
#' 
#' 
#' @examples
#' 
#' 
#' x <- Sys.Date()
#' 
#' 
#' out <- monthx(x)
#' 
#' out2 <- monthx(x,start_day=16)
#' 
#' 



monthx <- function(x,start_day=1) { ## TO DO 
  
  
  mr <- month(x)
  yr <- year(x)
  start_date <- sprintf("%04d-%02d-%02d",yr,mr,start_day)
  out <- mr
  im <- which(x<start_date)
  out[im] <- mr[im]-1
  
  out[out==0] <- 12
  
  return(out)
  
  
}