#' @export
#' @rdname a_geom_histogram
a_geom_freqpoly <- function(mapping = NULL, data = NULL,
                          a_stat = "bin", a_position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {

  params <- list(na.rm = na.rm, ...)
  if (identical(a_stat, "bin")) {
    params$pad <- TRUE
  }

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomPath,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = params
  )
}
