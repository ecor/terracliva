NULL
#' Sets months naturally sequatial
#' 
#'@param x vector of months
#'@param ... further agruments (not yet used)
#'
#'@export
#' 
#'@examples 
#'set_months_sequential(c(4,2,3,1))
#'set_months_sequential(c(12,4,2,3,11,1))
set_months_sequential <- function(x,...) {
  
  x <- unique(x)
  x <- x[x %in% 1:12]
  
  x <- sort(x)
  #ok <- array(1,length(x))
  out <- x
  
  dd <- c(1,diff(x))
  uu <- which(dd!=1)
  if (length(uu)>1) {
    stop("months vector not valid (not sequantial)")
  } else if (length(uu)==1) {
    iu <- uu:length(x)
    x[-iu] <- x[-iu]+12
    x <- sort(x)
    x[x>12] <- x[x>12]-12
  }
  
  
  return(x)
 
  
}