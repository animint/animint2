#' Summarise y values at unique/binned x x.
#'
#' \code{a_stat_summary} operates on unique \code{x}; \code{a_stat_summary_bin}
#' operators on binned \code{x}. They are more flexible versions of
#' \code{\link{a_stat_bin}}: instead of just counting, they can compute any
#' aggregate.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_stat", "summary")}
#'
#' @seealso \code{\link{a_geom_errorbar}}, \code{\link{a_geom_pointrange}},
#'  \code{\link{a_geom_linerange}}, \code{\link{a_geom_crossbar}} for geoms to
#'  display summarised data
#' @inheritParams a_stat_identity
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
#' \code{\link{a_mean_se}}.
#'
#' @param fun.data A function that is given the complete data and should
#'   return a data frame with variables \code{ymin}, \code{y}, and \code{ymax}.
#' @param fun.ymin,fun.y,fun.ymax Alternatively, supply three individual
#'   functions that are each passed a vector of x's and should return a
#'   single number.
#' @param fun.args Optional additional arguments passed on to the functions.
#' @export
#' @examples
#' d <- a_plot(mtcars, a_aes(cyl, mpg)) + a_geom_point()
#'
#' # You can supply individual functions to summarise the value at
#' # each x:
#' d + a_stat_summary(fun.y = "median", colour = "red", size = 2, a_geom = "point")
#' d + a_stat_summary(fun.y = "mean", colour = "red", size = 2, a_geom = "point")
#' d + a_aes(colour = factor(vs)) + a_stat_summary(fun.y = mean, a_geom="line")
#'
#' d + a_stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max,
#'   colour = "red")
#'
#' d <- a_plot(diamonds, a_aes(cut))
#' d + a_geom_bar()
#' d + a_stat_summary_bin(a_aes(y = price), fun.y = "mean", a_geom = "bar")
#'
#' \donttest{
#' # Don't use ylim to zoom into a summary plot - this throws the
#' # data away
#' p <- a_plot(mtcars, a_aes(cyl, mpg)) +
#'   a_stat_summary(fun.y = "mean", a_geom = "point")
#' p
#' p + ylim(15, 30)
#' # Instead use a_coord_cartesian
#' p + a_coord_cartesian(ylim = c(15, 30))
#'
#' # A set of useful summary functions is provided from the Hmisc package:
#' a_stat_sum_df <- function(fun, a_geom="crossbar", ...) {
#'   a_stat_summary(fun.data = fun, colour = "red", a_geom = a_geom, width = 0.2, ...)
#' }
#' d <- a_plot(mtcars, a_aes(cyl, mpg)) + a_geom_point()
#' # The crossbar a_geom needs grouping to be specified when used with
#' # a continuous x axis.
#' d + a_stat_sum_df("a_mean_cl_boot", mapping = a_aes(group = cyl))
#' d + a_stat_sum_df("a_mean_sdl", mapping = a_aes(group = cyl))
#' d + a_stat_sum_df("a_mean_sdl", fun.args = list(mult = 1), mapping = a_aes(group = cyl))
#' d + a_stat_sum_df("a_median_hilow", mapping = a_aes(group = cyl))
#'
#' # An example with highly skewed distributions:
#' if (require("ggplot2movies")) {
#' set.seed(596)
#' mov <- movies[sample(nrow(movies), 1000), ]
#'  m2 <- a_plot(mov, a_aes(x = factor(round(rating)), y = votes)) + a_geom_point()
#'  m2 <- m2 + a_stat_summary(fun.data = "a_mean_cl_boot", a_geom = "crossbar",
#'                          colour = "red", width = 0.3) + xlab("rating")
#' m2
#' # Notice how the overplotting skews off visual perception of the mean
#' # supplementing the raw data with summary statistics is _very_ important
#'
#' # Next, we'll look at votes on a log scale.
#'
#' # Transforming the scale means the data are transformed
#' # first, after which statistics are computed:
#' m2 + a_scale_y_log10()
#' # Transforming the coordinate system occurs after the
#' # statistic has been computed. This means we're calculating the summary on the raw data
#' # and stretching the a_geoms onto the log scale.  Compare the widths of the
#' # standard errors.
#' m2 + a_coord_trans(y="log10")
#' }
#' }
a_stat_summary <- function(mapping = NULL, data = NULL,
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
    a_stat = a_StatSummary,
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
a_StatSummary <- a_ggproto("a_StatSummary", a_Stat,
  required_aes = c("x", "y"),

  compute_panel = function(data, scales, fun.data = NULL, fun.y = NULL,
                     fun.ymax = NULL, fun.ymin = NULL, fun.args = list(),
                     na.rm = FALSE) {

    fun <- a_make_summary_fun(fun.data, fun.y, fun.ymax, fun.ymin, fun.args)
    summarise_by_x(data, fun)
  }
)

# Summarise a data.frame by parts
# Summarise a data frame by unique value of x
#
# This function is used by \code{\link{a_stat_summary}} to break a
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
#' with \code{\link{a_stat_summary}}.
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

a_wrap_hmisc <- function(fun) {

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
a_mean_cl_boot <- a_wrap_hmisc("smean.cl.boot")
#' @export
#' @rdname hmisc
a_mean_cl_normal <- a_wrap_hmisc("smean.cl.normal")
#' @export
#' @rdname hmisc
a_mean_sdl <- a_wrap_hmisc("smean.sdl")
#' @export
#' @rdname hmisc
a_median_hilow <- a_wrap_hmisc("smedian.hilow")

#' Calculate mean and standard errors on either side.
#'
#' @param x numeric vector
#' @param mult number of multiples of standard error
#' @seealso for use with \code{\link{a_stat_summary}}
#' @export
a_mean_se <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x) / length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - se, ymax = mean + se)
}
