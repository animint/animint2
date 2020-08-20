#' @export
#' @rdname geom_histogram
geom_freqpoly <- function(mapping = NULL, data = NULL,
                          stat = "bin", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  params <- list(na.rm = na.rm, ...)
  if (identical(stat, "bin")) {
    params$pad <- TRUE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFreqpoly,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname animint2-gganimintproto
#' @format NULL
#' @usage NULL
#' @export
GeomFreqpoly <- gganimintproto("GeomFreqpoly", GeomPath,
  pre_process = function(g, g.data, ...) {
    g$geom <- "line"
    g.data <- g.data[order(g.data$x), ]
    return(list(g = g, g.data = g.data))
  }
)
