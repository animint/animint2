#' @export
#' @rdname a_stat_summary_2d
#' @inheritParams a_stat_bin_hex
a_stat_summary_hex <- function(mapping = NULL, data = NULL,
                             a_geom = "hex", position = "identity",
                             ...,
                             bins = 30,
                             binwidth = NULL,
                             drop = TRUE,
                             fun = "mean",
                             fun.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatSummaryHex,
    a_geom = a_geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      fun = fun,
      fun.args = fun.args,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatSummaryHex <- a_ggproto("a_StatSummaryHex", a_Stat,
  default_aes = aes(fill = ..value..),

  required_aes = c("x", "y", "z"),

  compute_group = function(data, scales, binwidth = NULL, bins = 30, drop = TRUE,
                           fun = "mean", fun.args = list()) {
    try_require("hexbin", "a_stat_summary_hex")

    binwidth <- binwidth %||% hex_binwidth(bins, scales)
    hexBinSummarise(data$x, data$y, data$z, binwidth,
      fun = fun, fun.args = fun.args, drop = drop)
  }
)
