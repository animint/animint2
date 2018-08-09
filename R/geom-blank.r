#' Blank, draws nothing.
#'
#' The blank geom draws nothing, but can be a useful way of ensuring common
#' scales between different plots.
#'
#' @export
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @examples
#' a_plot(mtcars, a_aes(wt, mpg))
#' # Nothing to see here!
a_geom_blank <- function(mapping = NULL, data = NULL,
                       a_stat = "identity", a_position = "identity",
                       ...,
                       show.legend = NA,
                       inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomBlank,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(...)
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomBlank <- a_ggproto("a_GeomBlank", a_Geom,
  default_aes = a_aes(),
  handle_na = function(data, params) data,
  draw_panel = function(...) nullGrob()
)
