#' Add quantile lines from a quantile regression.
#'
#' This can be used as a continuous analogue of a a_geom_boxplot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "quantile")}
#'
#' @export
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @inheritParams a_geom_path
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by \code{method}.
#' @param a_geom,a_stat Use to override the default connection between
#'   \code{a_geom_quantile} and \code{a_stat_quantile}.
#' @examples
#' m <- a_plot(mpg, a_aes(displ, 1 / hwy)) + a_geom_point()
#' m + a_geom_quantile()
#' m + a_geom_quantile(quantiles = 0.5)
#' q10 <- seq(0.05, 0.95, by = 0.05)
#' m + a_geom_quantile(quantiles = q10)
#'
#' # You can also use rqss to fit smooth quantiles
#' m + a_geom_quantile(method = "rqss")
#' # Note that rqss doesn't pick a smoothing constant automatically, so
#' # you'll need to tweak lambda yourself
#' m + a_geom_quantile(method = "rqss", lambda = 0.1)
#'
#' # Set aesthetics to fixed value
#' m + a_geom_quantile(colour = "red", size = 2, alpha = 0.5)
a_geom_quantile <- function(mapping = NULL, data = NULL,
                          a_stat = "quantile", a_position = "identity",
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
    a_geom = a_GeomQuantile,
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
a_GeomQuantile <- a_ggproto("a_GeomQuantile", a_GeomPath,
  default_aes = defaults(
    a_aes(weight = 1, colour = "#3366FF", size = 0.5),
    a_GeomPath$default_aes
  )
)
