#' Alpha scales.
#'
#' \code{a_scale_alpha} is an alias for \code{a_scale_alpha_continuous} since
#' that is the most common use of alpha, and it saves a bit of typing.
#'
#' @param ... Other arguments passed on to \code{\link{continuous_a_scale}}
#'   or \code{\link{discrete_a_scale}} as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range range of output alpha values.  Should lie between 0 and 1.
#' @export
#' @examples
#' (p <- a_plot(mtcars, a_aes(mpg, cyl)) +
#'   a_geom_point(a_aes(alpha = cyl)))
#' p + a_scale_alpha("cylinders")
#' p + a_scale_alpha("number\nof\ncylinders")
#'
#' p + a_scale_alpha(range = c(0.4, 0.8))
#'
#' (p <- a_plot(mtcars, a_aes(mpg, cyl)) +
#'   a_geom_point(a_aes(alpha = factor(cyl))))
#' p + a_scale_alpha_discrete(range = c(0.4, 0.8))
a_scale_alpha <- function(..., range = c(0.1, 1)) {
  continuous_a_scale("alpha", "alpha_c", rescale_pal(range), ...)
}

#' @rdname a_scale_alpha
#' @export
a_scale_alpha_continuous <- a_scale_alpha

#' @rdname a_scale_alpha
#' @export
a_scale_alpha_discrete <- function(..., range = c(0.1, 1)) {
  discrete_a_scale("alpha", "alpha_d",
    function(n) seq(range[1], range[2], length.out = n), ...)
}
