#' Display contours of a 3d surface in 2d.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "contour")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @inheritParams a_geom_path
#' @seealso \code{\link{a_geom_density_2d}}: 2d density contours
#' @export
#' @export
#' @examples
#' #' # Basic plot
#' v <- a_plot(faithfuld, a_aes(waiting, eruptions, z = density))
#' v + a_geom_contour()
#'
#' # Or compute from raw data
#' a_plot(faithful, a_aes(waiting, eruptions)) +
#'   a_geom_density_2d()
#'
#' \donttest{
#' # Setting bins creates evenly spaced contours in the range of the data
#' v + a_geom_contour(bins = 2)
#' v + a_geom_contour(bins = 10)
#'
#' # Setting binwidth does the same thing, parameterised by the distance
#' # between contours
#' v + a_geom_contour(binwidth = 0.01)
#' v + a_geom_contour(binwidth = 0.001)
#'
#' # Other parameters
#' v + a_geom_contour(a_aes(colour = ..level..))
#' v + a_geom_contour(colour = "red")
#' v + a_geom_raster(a_aes(fill = density)) +
#'   a_geom_contour(colour = "white")
#' }
a_geom_contour <- function(mapping = NULL, data = NULL,
                         a_stat = "contour", a_position = "identity",
                         ...,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 1,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomContour,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
a_GeomContour <- a_ggproto("a_GeomContour", a_GeomPath,
  default_aes = a_aes(weight = 1, colour = "#3366FF", size = 0.5, linetype = 1,
    alpha = NA)
)
