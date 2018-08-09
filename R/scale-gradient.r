#' Smooth gradient between two colours
#'
#' \code{a_scale_*_gradient} creates a two colour gradient (low-high),
#' \code{a_scale_*_gradient2} creates a diverging colour gradient (low-mid-high),
#' \code{a_scale_*_gradientn} creats a n-colour gradient.
#'
#' Default colours are generated with \pkg{munsell} and
#' \code{mnsl(c("2.5PB 2/4", "2.5PB 7/10")}. Generally, for continuous
#' colour scales you want to keep hue constant, but vary chroma and
#' luminance. The \pkg{munsell} package makes this easy to do using the
#' Munsell colour system.
#'
#' @inheritParams scales::seq_gradient_pal
#' @inheritParams a_scale_colour_hue
#' @param low,high Colours for low and high ends of the gradient.
#' @param a_guide Type of legend. Use \code{"colourbar"} for continuous
#'   colour bar, or \code{"legend"} for discrete colour legend.
#' @seealso \code{\link[scales]{seq_gradient_pal}} for details on underlying
#'   palette
#' @seealso Other colour scales:
#'   \code{\link{a_scale_colour_brewer}},
#'   \code{\link{a_scale_colour_grey}},
#'   \code{\link{a_scale_colour_hue}}
#' @rdname a_scale_gradient
#' @export
#' @examples
#' df <- data.frame(
#'   x = runif(100),
#'   y = runif(100),
#'   z1 = rnorm(100),
#'   z2 = abs(rnorm(100))
#' )
#'
#' # Default colour scale colours from light blue to dark blue
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point(a_aes(colour = z2))
#'
#' # For diverging colour scales use gradient2
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point(a_aes(colour = z1)) +
#'   a_scale_colour_gradient2()
#'
#' # Use your own colour scale with gradientn
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point(a_aes(colour = z1)) +
#'   a_scale_colour_gradientn(colours = terrain.colors(10))
#'
#' # Equivalent fill scales do the same job for the fill aesthetic
#' a_plot(faithfuld, a_aes(waiting, eruptions)) +
#'   a_geom_raster(a_aes(fill = density)) +
#'   a_scale_fill_gradientn(colours = terrain.colors(10))
#'
#' # Adjust colour choices with low and high
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point(a_aes(colour = z2)) +
#'   a_scale_colour_gradient(low = "white", high = "black")
#' # Avoid red-green colour contrasts because ~10% of men have difficulty
#' # seeing them
a_scale_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", a_guide = "colourbar") {
  continuous_a_scale("colour", "gradient", seq_gradient_pal(low, high, space),
    na.value = na.value, a_guide = a_guide, ...)
}

#' @rdname a_scale_gradient
#' @export
a_scale_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", a_guide = "colourbar") {
  continuous_a_scale("fill", "gradient", seq_gradient_pal(low, high, space),
    na.value = na.value, a_guide = a_guide, ...)
}

#' @inheritParams scales::div_gradient_pal
#' @param midpoint The midpoint (in data value) of the diverging scale.
#'   Defaults to 0.
#' @rdname a_scale_gradient
#' @export
a_scale_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", a_guide = "colourbar") {
  continuous_a_scale("colour", "gradient2",
    div_gradient_pal(low, mid, high, space), na.value = na.value, a_guide = a_guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname a_scale_gradient
#' @export
a_scale_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0, space = "Lab", na.value = "grey50", a_guide = "colourbar") {
  continuous_a_scale("fill", "gradient2",
    div_gradient_pal(low, mid, high, space), na.value = na.value, a_guide = a_guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}

#' @inheritParams scales::gradient_n_pal
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @rdname a_scale_gradient
#' @export
a_scale_colour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", a_guide = "colourbar", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_a_scale("colour", "gradientn",
    gradient_n_pal(colours, values, space), na.value = na.value, a_guide = a_guide, ...)
}
#' @rdname a_scale_gradient
#' @export
a_scale_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50", a_guide = "colourbar", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_a_scale("fill", "gradientn",
    gradient_n_pal(colours, values, space), na.value = na.value, a_guide = a_guide, ...)
}
