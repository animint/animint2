#' A line segment parameterised by location, direction and distance.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "spoke")}
#'
#' @inheritParams layer
#' @inheritParams geom_segment
#' @export
#' @examples
#' df <- expand.grid(x = 1:10, y=1:10)
#' df$angle <- runif(100, 0, 2*pi)
#' df$speed <- runif(100, 0, sqrt(0.1 * df$x))
#'
#' a_plot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = angle), radius = 0.5)
#'
#' a_plot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = angle, radius = speed))
geom_spoke <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = a_GeomSpoke,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_spoke
#' @usage NULL
stat_spoke <- function(...) {
  message("stat_spoke is deprecated, please use geom_spoke")
  geom_spoke(...)
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
