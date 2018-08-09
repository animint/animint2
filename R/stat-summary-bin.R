#' @rdname a_stat_summary
#' @inheritParams a_stat_bin
#' @export
a_stat_summary_bin <- function(mapping = NULL, data = NULL,
                             a_geom = "pointrange", a_position = "identity",
                             ...,
                             fun.data = NULL,
                             fun.y = NULL,
                             fun.ymax = NULL,
                             fun.ymin = NULL,
                             fun.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatSummaryBin,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      fun.data = fun.data,
      fun.y = fun.y,
      fun.ymax = fun.ymax,
      fun.ymin = fun.ymin,
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
a_StatSummaryBin <- a_ggproto("a_StatSummaryBin", a_Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, fun.data = NULL, fun.y = NULL,
                           fun.ymax = NULL, fun.ymin = NULL, fun.args = list(),
                           bins = 30, binwidth = NULL, origin = NULL, right = FALSE,
                           na.rm = FALSE) {

    fun <- a_make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)

    breaks <- bin2d_breaks(scales$x, NULL, origin, binwidth, bins, right = right)

    data$bin <- cut(data$x, breaks, include.lowest = TRUE, labels = FALSE)
    out <- plyr::ddply(data, "bin", fun)

    locs <- bin_loc(breaks, out$bin)
    out$x <- locs$mid
    out$width <- if (scales$x$is_discrete()) 0.9 else locs$length
    out
  }
)

a_make_summary_fun <- function(fun.data, fun.y, fun.ymax, fun.ymin, fun.args) {
  if (!is.null(fun.data)) {
    # Function that takes complete data frame as input
    fun.data <- match.fun(fun.data)
    function(df) {
      do.call(fun.data, c(list(quote(df$y)), fun.args))
    }
  } else if (!is.null(fun.y) || !is.null(fun.ymax) || !is.null(fun.ymin)) {
    # Three functions that take vectors as inputs

    call_f <- function(fun, x) {
      if (is.null(fun)) return(NA_real_)
      do.call(fun, c(list(quote(x)), fun.args))
    }

    function(df, ...) {
      data.frame(
        ymin = call_f(fun.ymin, df$y),
        y = call_f(fun.y, df$y),
        ymax = call_f(fun.ymax, df$y)
      )
    }
  } else {
    message("No summary function supplied, defaulting to `a_mean_se()")
    function(df) {
      a_mean_se(df$y)
    }
  }
}
