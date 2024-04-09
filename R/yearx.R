NULL
#'
#' Year starting from a generic date
#'
#' @param x Date
#' @param start_month first month of the considered Year. 
#' @param start_day first day of the month of the considered Year
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
#' out <- yearx(x)
#' 
#' out2 <- yearx(x,start_month=6)
#' 
#' 
#'
yearx <- function(x,start_month=1,start_day=1) {
  
  
  yr <- year(x)
  
  start_date <- sprintf("%04d-%02d-%02d",yr,start_month,start_day)
  
  out <- yr
  out[x<start_date] <- yr[x<start_date]-1
  
  
  
  return(out)
  
  
}