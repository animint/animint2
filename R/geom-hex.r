#' Hexagon binning.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "hex")}
#'
#' @seealso \code{\link{a_stat_bin2d}} for rectangular binning
#' @param a_geom,a_stat Override the default connection between \code{a_geom_hex} and
#'   \code{a_stat_binhex.}
#' @export
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' d <- a_plot(diamonds, a_aes(carat, price))
#' d + a_geom_hex()
#'
#' \donttest{
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + a_geom_hex(bins = 10)
#' d + a_geom_hex(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + a_geom_hex(binwidth = c(1, 1000))
#' d + a_geom_hex(binwidth = c(.1, 500))
#' }
a_geom_hex <- function(mapping = NULL, data = NULL,
                     a_stat = "binhex", a_position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomHex,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomHex <- a_ggproto("a_GeomHex", a_Geom,
  draw_group = function(data, panel_scales, a_coord) {
    if (!inherits(a_coord, "a_CoordCartesian")) {
      stop("a_geom_hex() only works with Cartesian coordinates", call. = FALSE)
    }

    a_coord <- a_coord$transform(data, panel_scales)
    ggname("geom_hex", hexGrob(
      a_coord$x, a_coord$y, colour = a_coord$colour,
      fill = alpha(a_coord$fill, a_coord$alpha)
    ))
  },

  required_aes = c("x", "y"),

  default_aes = a_aes(colour = NA, fill = "grey50", size = 0.5, alpha = NA),

  draw_key = a_draw_key_polygon
)


# Draw hexagon grob
# Modified from code by Nicholas Lewin-Koh and Martin Maechler
#
# @param x positions of hex centres
# @param y positions
# @param vector of hex sizes
# @param border colour
# @param fill colour
# @keyword internal
hexGrob <- function(x, y, size = rep(1, length(x)), colour = "grey50", fill = "grey90") {
  stopifnot(length(y) == length(x))

  dx <- resolution(x, FALSE)
  dy <- resolution(y, FALSE) / sqrt(3) / 2 * 1.15

  hexC <- hexbin::hexcoords(dx, dy, n = 1)

  n <- length(x)

  polygonGrob(
    x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(x, each = 6),
    y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(y, each = 6),
    default.units = "native",
    id.lengths = rep(6, n), gp = gpar(col = colour, fill = fill)
  )
}
