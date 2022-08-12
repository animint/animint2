#' Summarise y values at unique/binned x x.
#'
#' \code{stat_summary} operates on unique \code{x}; \code{stat_summary_bin}
#' operators on binned \code{x}. They are more flexible versions of
#' \code{\link{stat_bin}}: instead of just counting, they can compute any
#' aggregate.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("stat", "summary")}
#'
#' @seealso \code{\link{geom_errorbar}}, \code{\link{geom_pointrange}},
#'  \code{\link{geom_linerange}}, \code{\link{geom_crossbar}} for geoms to
#'  display summarised data
#' @inheritParams stat_identity
#' @section Summary functions:
#' You can either supply summary functions individually (\code{fun.y},
#' \code{fun.ymax}, \code{fun.ymin}), or as a single function (\code{fun.data}):
#'
#' \describe{
#'   \item{fun.data}{Complete summary function. Should take numeric vector as
#'      input and return data frame as output}
#'   \item{fun.ymin}{ymin summary function (should take numeric vector and
#'     return single number)}
#'   \item{fun.y}{y summary function (should take numeric vector and return
#'     single number)}
#'   \item{fun.ymax}{ymax summary function (should take numeric vector and
#'     return single number)}
#' }
#'
#' A simple vector function is easiest to work with as you can return a single
#' number, but is somewhat less flexible. If your summary function computes
#' multiple values at once (e.g. ymin and ymax), use \code{fun.data}.
#'
#' If no aggregation functions are suppled, will default to
#' \code{\link{mean_se}}.
#'
#' @param fun.data A function that is given the complete data and should
#'   return a data frame with variables \code{ymin}, \code{y}, and \code{ymax}.
#' @param fun.ymin,fun.y,fun.ymax Alternatively, supply three individual
#'   functions that are each passed a vector of x's and should return a
#'   single number.
#' @param fun.args Optional additional arguments passed on to the functions.
#' @export
stat_summary <- function(mapping = NULL, data = NULL,
                         geom = "pointrange", position = "identity",
                         ...,
                         fun.data = NULL,
                         fun.y = NULL,
                         fun.ymax = NULL,
                         fun.ymin = NULL,
                         fun.args = list(),
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummary,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
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

#' @rdname animint2-gganimintproto
#' @format NULL
#' @usage NULL
#' @export
StatSummary <- gganimintproto("StatSummary", Stat,
  required_aes = c("x", "y"),

  compute_panel = function(data, scales, fun.data = NULL, fun.y = NULL,
                     fun.ymax = NULL, fun.ymin = NULL, fun.args = list(),
                     na.rm = FALSE) {

    fun <- make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)
    summarise_by_x(data, fun)
  }
)

# Summarise a data.frame by parts
# Summarise a data frame by unique value of x
#
# This function is used by \code{\link{stat_summary}} to break a
# data.frame into pieces, summarise each piece, and join the pieces
# back together, retaining original columns unaffected by the summary.
#
# @param \code{\link{data.frame}} to summarise
# @param vector to summarise by
# @param summary function (must take and return a data.frame)
# @param other arguments passed on to summary function
# @keyword internal
summarise_by_x <- function(data, summary, ...) {
  summary <- plyr::ddply(data, c("group", "x"), summary, ...)
  unique <- plyr::ddply(data, c("group", "x"), uniquecols)
  unique$y <- NULL

  merge(summary, unique, by = c("x", "group"), sort = FALSE)
}

#' Wrap up a selection of summary functions from Hmisc to make it easy to use
#' with \code{\link{stat_summary}}.
#'
#' See the Hmisc documentation for details of their options.
#'
#' @param x a numeric vector
#' @param ... other arguments passed on to the respective Hmisc function.
#' @seealso \code{\link[Hmisc]{smean.cl.boot}},
#'   \code{\link[Hmisc]{smean.cl.normal}}, \code{\link[Hmisc]{smean.sdl}},
#'    \code{\link[Hmisc]{smedian.hilow}}
#' @name hmisc
NULL

wrap_hmisc <- function(fun) {

  function(x, ...) {
    if (!requireNamespace("Hmisc", quietly = TRUE))
      stop("Hmisc package required for this function", call. = FALSE)

    fun <- getExportedValue("Hmisc", fun)
    result <- do.call(fun, list(x = quote(x), ...))

    plyr::rename(
      data.frame(t(result)),
      c(Median = "y", Mean = "y", Lower = "ymin", Upper = "ymax"),
      warn_missing = FALSE
    )
  }
}
#' @export
#' @rdname hmisc
mean_cl_boot <- wrap_hmisc("smean.cl.boot")
#' @export
#' @rdname hmisc
mean_cl_normal <- wrap_hmisc("smean.cl.normal")
#' @export
#' @rdname hmisc
mean_sdl <- wrap_hmisc("smean.sdl")
#' @export
#' @rdname hmisc
median_hilow <- wrap_hmisc("smedian.hilow")

#' Calculate mean and standard errors on either side.
#'
#' @param x numeric vector
#' @param mult number of multiples of standard error
#' @seealso for use with \code{\link{stat_summary}}
#' @export
mean_se <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x) / length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - se, ymax = mean + se)
}
