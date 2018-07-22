#' Identity statistic.
#'
#' The identity statistic leaves the data unchanged.
#'
#' @inheritParams layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' p <- a_plot(mtcars, aes(wt, mpg))
#' p + a_stat_identity()
a_stat_identity <- function(mapping = NULL, data = NULL,
                          a_geom = "point", position = "identity",
                          ...,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatIdentity,
    a_geom = a_geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatIdentity <- a_ggproto("a_StatIdentity", a_Stat,
  compute_layer = function(data, scales, params) {
    data
  }
)
