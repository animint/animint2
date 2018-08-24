#' Compute the "a_resolution" of a data vector.
#'
#' The a_resolution is is the smallest non-zero distance between adjacent
#' values.  If there is only one unique value, then the a_resolution is defined
#' to be one.
#'
#' If x is an integer vector, then it is assumed to represent a discrete
#' variable, and the a_resolution is 1.
#'
#' @param x numeric vector
#' @param zero should a zero value be automatically included in the
#'   computation of a_resolution
#' @export
#' @examples
#' a_resolution(1:10)
#' a_resolution((1:10) - 0.5)
#' a_resolution((1:10) - 0.5, FALSE)
#' a_resolution(c(1,2, 10, 20, 50))
#' a_resolution(as.integer(c(1, 10, 20, 50)))  # Returns 1
a_resolution <- function(x, zero = TRUE) {
  if (is.integer(x) || zero_range(range(x, na.rm = TRUE)))
    return(1)

  x <- unique(as.numeric(x))
  if (zero) {
    x <- unique(c(0, x))
  }

  min(diff(sort(x)))
}
