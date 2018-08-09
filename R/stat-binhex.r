#' @export
#' @rdname a_geom_hex
#' @inheritParams a_stat_bin_2d
a_stat_bin_hex <- function(mapping = NULL, data = NULL,
                         a_geom = "hex", a_position = "identity",
                         ...,
                         bins = 30,
                         binwidth = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatBinhex,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname a_geom_hex
#' @usage NULL
a_stat_binhex <- a_stat_bin_hex

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatBinhex <- a_ggproto("a_StatBinhex", a_Stat,
  default_aes = a_aes(fill = ..value..),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           na.rm = FALSE) {
    try_require("hexbin", "a_stat_binhex")

    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    wt <- data$weight %||% rep(1L, nrow(data))
    hexBinSummarise(data$x, data$y, wt, binwidth, sum)
  }
)
