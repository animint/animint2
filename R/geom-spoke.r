#' A line segment parameterised by location, direction and distance.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "spoke")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_segment
#' @export
#' @examples
#' df <- expand.grid(x = 1:10, y=1:10)
#' df$angle <- runif(100, 0, 2*pi)
#' df$speed <- runif(100, 0, sqrt(0.1 * df$x))
#'
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point() +
#'   a_geom_spoke(a_aes(angle = angle), radius = 0.5)
#'
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point() +
#'   a_geom_spoke(a_aes(angle = angle, radius = speed))
a_geom_spoke <- function(mapping = NULL, data = NULL,
                       a_stat = "identity", a_position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_geom = a_GeomSpoke,
    a_stat = a_stat,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname a_geom_spoke
#' @usage NULL
a_stat_spoke <- function(...) {
  message("a_stat_spoke is deprecated, please use a_geom_spoke")
  a_geom_spoke(...)
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomSpoke <- a_ggproto("a_GeomSpoke", a_GeomSegment,
  setup_data = function(data, params) {
    data$radius <- data$radius %||% params$radius
    data$angle <- data$angle %||% params$angle

    transform(data,
      xend = x + cos(angle) * radius,
      yend = y + sin(angle) * radius
    )
  },
  required_aes = c("x", "y", "angle", "radius")
)
