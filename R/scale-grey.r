#' Sequential grey colour scale.
#'
#' Based on \code{\link{gray.colors}}
#'
#' @inheritParams scales::grey_pal
#' @inheritParams a_scale_colour_hue
#' @seealso Other colour scales:
#'   \code{\link{a_scale_colour_brewer}},
#'   \code{\link{a_scale_colour_gradient}},
#'   \code{\link{a_scale_colour_hue}}
#' @rdname a_scale_grey
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point(a_aes(colour = factor(cyl)))
#' p + a_scale_colour_grey()
#' p + a_scale_colour_grey(end = 0)
#'
#' # You may want to turn off the pale grey background with this scale
#' p + a_scale_colour_grey() + a_theme_bw()
#'
#' # Colour of missing values is controlled with na.value:
#' miss <- factor(sample(c(NA, 1:5), nrow(mtcars), replace = TRUE))
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(colour = miss)) +
#'   a_scale_colour_grey()
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(colour = miss)) +
#'   a_scale_colour_grey(na.value = "green")
a_scale_colour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red") {
  discrete_a_scale("colour", "grey", grey_pal(start, end),
    na.value = na.value, ...)
}

#' @rdname a_scale_grey
#' @export
a_scale_fill_grey <- function(..., start = 0.2, end = 0.8, na.value = "red") {
  discrete_a_scale("fill", "grey", grey_pal(start, end),
    na.value = na.value, ...)
}
