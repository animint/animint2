#' Draw rectangles.
#'
#' \code{a_geom_rect} and \code{a_geom_tile} do the same thing, but are
#' parameterised differently. \code{a_geom_rect} uses the locations of the four
#' corners (\code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}).
#' \code{a_geom_tile} uses the center of the tile and its size (\code{x},
#' \code{y}, \code{width}, \code{height}). \code{a_geom_raster} is a high
#' performance special case for when all the tiles are the same size.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "tile")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' # The most common use for rectangles is to draw a surface. You always want
#' # to use a_geom_raster here because it's so much faster, and produces
#' # smaller output when saving to PDF
#' a_plot(faithfuld, a_aes(waiting, eruptions)) +
#'  a_geom_raster(a_aes(fill = density))
#'
#' # Interpolation smooths the surface & is most helpful when rendering images.
#' a_plot(faithfuld, a_aes(waiting, eruptions)) +
#'  a_geom_raster(a_aes(fill = density), interpolate = TRUE)
#'
#' # If you want to draw arbitrary rectangles, use a_geom_tile() or a_geom_rect()
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_tile(a_aes(fill = z))
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_tile(a_aes(fill = z, width = w), colour = "grey50")
#' a_plot(df, a_aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   a_geom_rect(a_aes(fill = z, width = w), colour = "grey50")
#'
#' \donttest{
#' # Justification controls where the cells are anchored
#' df <- expand.grid(x = 0:5, y = 0:5)
#' df$z <- runif(nrow(df))
#' # default is compatible with a_geom_tile()
#' a_plot(df, a_aes(x, y, fill = z)) + a_geom_raster()
#' # zero padding
#' a_plot(df, a_aes(x, y, fill = z)) + a_geom_raster(hjust = 0, vjust = 0)
#'
#' # Inspired by the image-density plots of Ken Knoblauch
#' cars <- a_plot(mtcars, a_aes(mpg, factor(cyl)))
#' cars + a_geom_point()
#' cars + a_stat_bin2d(a_aes(fill = ..count..), binwidth = c(3,1))
#' cars + a_stat_bin2d(a_aes(fill = ..density..), binwidth = c(3,1))
#'
#' cars + a_stat_density(a_aes(fill = ..density..), a_geom = "raster", a_position = "identity")
#' cars + a_stat_density(a_aes(fill = ..count..), a_geom = "raster", a_position = "identity")
#' }
a_geom_tile <- function(mapping = NULL, data = NULL,
                      a_stat = "identity", a_position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomTile,
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
#' @include geom-rect.r
a_GeomTile <- a_ggproto("a_GeomTile", a_GeomRect,
  extra_params = c("na.rm", "width", "height"),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = a_aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  draw_key = a_draw_key_polygon
)
