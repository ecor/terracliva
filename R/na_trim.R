#' Trim leading and/or trailing NA values from a vector
#'
#' Removes the \code{NA} values found at the beginning and/or at the end of a
#' vector, leaving any internal \code{NA} untouched. This is useful, for
#' instance, to cut a time series to the period actually covered by data
#' before computing statistics or fitting models.
#'
#' @param x an atomic vector (numeric, integer, logical, character, ...).
#' @param sides character string indicating which side(s) to trim. One of
#'   \code{"both"} (default), \code{"left"} (leading \code{NA}s only) or
#'   \code{"right"} (trailing \code{NA}s only).
#'
#' @return A vector of the same type as \code{x}, with leading and/or trailing
#'   \code{NA}s removed. Names, if present, are preserved. If \code{x} contains
#'   only \code{NA} values, a zero-length vector of the same type is returned.
#'
#' @seealso \code{\link{is.na}}, \code{\link{rle}}
#'
#' @export
#'
#' @examples
#' x <- c(NA, NA, 3, NA, 5, 6, NA)
#'
#' na_trim(x)
#' na_trim(x, sides = "left")
#' na_trim(x, sides = "right")
#'
#' ## all-NA vector
#' na_trim(c(NA_real_, NA_real_))
#'
#' ## named vector: names are kept
#' y <- c(a = NA, b = 1, c = NA, d = 2, e = NA)
#' na_trim(y)
#'
na_trim <- function(x, sides = c("both", "left", "right")) {
  
  sides <- match.arg(sides)
  
  ok <- which(!is.na(x))
  if (length(ok) == 0) return(x[0])
  
  first <- if (sides %in% c("both", "left"))  ok[1]          else 1L
  last  <- if (sides %in% c("both", "right")) ok[length(ok)] else length(x)
  
  x[first:last]
}