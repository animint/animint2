#' Remove duplicates.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_stat", "unique")}
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' a_plot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1)
#' a_plot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1, stat="unique")
stat_unique <- function(mapping = NULL, data = NULL,
                        geom = "point", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = a_StatUnique,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
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
