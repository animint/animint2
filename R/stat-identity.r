#' Identity statistic.
#'
#' The identity statistic leaves the data unchanged.
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(wt, mpg))
#' p + a_stat_identity()
a_stat_identity <- function(mapping = NULL, data = NULL,
                          a_geom = "point", a_position = "identity",
                          ...,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatIdentity,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
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
