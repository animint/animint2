#' Sequential, diverging and qualitative colour scales from colorbrewer.org
#'
#' ColorBrewer provides sequential, diverging and qualitative colour schemes
#' which are particularly suited and tested to display discrete values (levels
#' of a factor) on a map. ggplot2 can use those colours in discrete scales. It
#' also allows to smoothly interpolate 6 colours from any palette to a
#' continuous scale (6 colours per palette gives nice gradients; more results in
#' more saturated colours which do not look as good). However, the original
#' colour schemes (particularly the qualitative ones) were not intended for this
#' and the perceptual result is left to the appreciation of the user.
#' See \url{http://colorbrewer2.org} for more information.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'   \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'   \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#'
#' @inheritParams scales::brewer_pal
#' @inheritParams a_scale_colour_hue
#' @inheritParams a_scale_colour_gradient
#' @inheritParams scales::gradient_n_pal
#' @seealso Other colour scales:
#'   \code{\link{a_scale_colour_gradient}},
#'   \code{\link{a_scale_colour_grey}},
#'   \code{\link{a_scale_colour_hue}}
#' @rdname a_scale_brewer
#' @export
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- a_plot(dsamp, a_aes(carat, price)) +
#'   a_geom_point(a_aes(colour = clarity)))
#'
#' # Change a_scale label
#' d + a_scale_colour_brewer()
#' d + a_scale_colour_brewer("Diamond\nclarity")
#'
#' # Select brewer palette to use, see ?scales::brewer_pal for more details
#' d + a_scale_colour_brewer(palette = "Greens")
#' d + a_scale_colour_brewer(palette = "Set1")
#'
#' \donttest{
#' # a_scale_fill_brewer works just the same as
#' # a_scale_colour_brewer but for fill colours
#' p <- a_plot(diamonds, a_aes(x = price, fill = cut)) +
#'   a_geom_histogram(a_position = "dodge", binwidth = 1000)
#' p + a_scale_fill_brewer()
#' # the order of colour can be reversed
#' p + a_scale_fill_brewer(direction = -1)
#' # the brewer scales look better on a darker background
#' p + a_scale_fill_brewer(direction = -1) + a_theme_dark()
#' }
#'
#' # Use distiller variant with continous data
#' v <- a_plot(faithfuld) +
#'   a_geom_tile(a_aes(waiting, eruptions, fill = density))
#' v
#' v + a_scale_fill_distiller()
#' v + a_scale_fill_distiller(palette = "Spectral")
a_scale_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1) {
  discrete_a_scale("colour", "brewer", brewer_pal(type, palette, direction), ...)
}

#' @export
#' @rdname a_scale_brewer
a_scale_fill_brewer <- function(..., type = "seq", palette = 1, direction = 1) {
  discrete_a_scale("fill", "brewer", brewer_pal(type, palette, direction), ...)
}

#' @export
#' @rdname a_scale_brewer
a_scale_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", a_guide = "colourbar") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  continuous_a_scale("colour", "distiller",
    gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, a_guide = a_guide, ...)
  # NB: 6 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
}

#' @export
#' @rdname a_scale_brewer
a_scale_fill_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", a_guide = "colourbar") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  continuous_a_scale("fill", "distiller",
    gradient_n_pal(brewer_pal(type, palette, direction)(6), values, space), na.value = na.value, a_guide = a_guide, ...)
}

# icon.brewer <- function() {
#   rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width = 0.21,
#     gp = gpar(fill = RColorBrewer::brewer.pal(5, "PuOr"), col = NA)
#   )
# }
