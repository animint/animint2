#' Blank, draws nothing.
#'
#' The blank geom draws nothing, but can be a useful way of ensuring common
#' scales between different plots.
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' a_plot(mtcars, aes(wt, mpg))
#' # Nothing to see here!
geom_blank <- function(mapping = NULL, data = NULL,
                       a_stat = "identity", position = "identity",
                       ...,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    geom = a_GeomBlank,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomBlank <- a_ggproto("a_GeomBlank", a_Geom,
  default_aes = aes(),
  handle_na = function(data, params) data,
  draw_panel = function(...) nullGrob()
)
