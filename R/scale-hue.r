#' Qualitative colour scale with evenly spaced hues.
#'
#' @param na.value Colour to use for missing values
#' @param ... Other arguments passed on to \code{\link{discrete_a_scale}}
#'   to control name, limits, breaks, labels and so forth.
#' @inheritParams scales::hue_pal
#' @rdname a_scale_hue
#' @export
#' @seealso Other colour scales:
#'   \code{\link{a_scale_colour_brewer}},
#'   \code{\link{a_scale_colour_gradient}},
#'   \code{\link{a_scale_colour_grey}}
#' @examples
#' \donttest{
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- a_plot(dsamp, a_aes(carat, price)) + a_geom_point(a_aes(colour = clarity)))
#'
#' # Change scale label
#' d + a_scale_colour_hue()
#' d + a_scale_colour_hue("clarity")
#' d + a_scale_colour_hue(expression(clarity[beta]))
#'
#' # Adjust luminosity and chroma
#' d + a_scale_colour_hue(l = 40, c = 30)
#' d + a_scale_colour_hue(l = 70, c = 30)
#' d + a_scale_colour_hue(l = 70, c = 150)
#' d + a_scale_colour_hue(l = 80, c = 150)
#'
#' # Change range of hues used
#' d + a_scale_colour_hue(h = c(0, 90))
#' d + a_scale_colour_hue(h = c(90, 180))
#' d + a_scale_colour_hue(h = c(180, 270))
#' d + a_scale_colour_hue(h = c(270, 360))
#'
#' # Vary opacity
#' # (only works with pdf, quartz and cairo devices)
#' d <- a_plot(dsamp, a_aes(carat, price, colour = clarity))
#' d + a_geom_point(alpha = 0.9)
#' d + a_geom_point(alpha = 0.5)
#' d + a_geom_point(alpha = 0.2)
#'
#' # Colour of missing values is controlled with na.value:
#' miss <- factor(sample(c(NA, 1:5), nrow(mtcars), replace = TRUE))
#' a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point(a_aes(colour = miss))
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(colour = miss)) +
#'   a_scale_colour_hue(na.value = "black")
#' }
a_scale_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = "grey50") {
  discrete_a_scale("colour", "hue", hue_pal(h, c, l, h.start, direction),
    na.value = na.value, ...)
}

#' @rdname a_scale_hue
#' @export
a_scale_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1, na.value = "grey50") {
  discrete_a_scale("fill", "hue", hue_pal(h, c, l, h.start, direction),
    na.value = na.value, ...)
}
