#' Remove duplicates.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_stat", "unique")}
#'
#' @export
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @examples
#' a_plot(mtcars, a_aes(vs, am)) + a_geom_point(alpha = 0.1)
#' a_plot(mtcars, a_aes(vs, am)) + a_geom_point(alpha = 0.1, a_stat="unique")
a_stat_unique <- function(mapping = NULL, data = NULL,
                        a_geom = "point", a_position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatUnique,
    a_geom = a_geom,
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
a_StatUnique <- a_ggproto("a_StatUnique", a_Stat,
  compute_panel = function(data, scales) unique(data)
)
