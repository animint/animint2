#' Convenience functions to set the axis limits.
#'
#' Observations not in this range will be dropped completely and
#' not passed to any other layers.  If a NA value is substituted for one of the
#' limits that limit is automatically calculated.
#'
#' @param ... If numeric, will create a continuous scale, if factor or
#'   character, will create a discrete scale.  For \code{lims}, every
#'   argument must be named.
#' @seealso For changing x or y axis limits \strong{without} dropping data
#'   observations, see \code{\link{a_coord_cartesian}}.
#' @export
#' @examples
#' # xlim
#' xlim(15, 20)
#' xlim(20, 15)
#' xlim(c(10, 20))
#' xlim("a", "b", "c")
#'
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point() +
#'   xlim(15, 20)
#' # with automatic lower limit
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point() +
#'   xlim(NA, 20)
#'
#' # Change both xlim and ylim
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point() +
#'   lims(x = c(10, 20), y = c(3, 5))
lims <- function(...) {
  args <- list(...)

  if (any(!has_name(args))) {
    stop("All arguments must be named", call. = FALSE)
  }

  Map(limits, args, names(args))
}

#' @export
#' @rdname lims
xlim <- function(...) {
  limits(c(...), "x")
}

#' @export
#' @rdname lims
ylim <- function(...) {
  limits(c(...), "y")
}

#' Generate correct scale type for specified limits
#'
#' @param lims vector of limits
#' @param var name of variable
#' @export
#' @examples
#' limits(c(1, 5), "x")
#' limits(c(5, 1), "x")
#' limits(c("A", "b", "c"), "x")
#' limits(c("A", "b", "c"), "fill")
#' limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
limits <- function(lims, var) UseMethod("limits")
#' @export
limits.numeric <- function(lims, var) {
  stopifnot(length(lims) == 2)
  if (!any(is.na(lims)) && lims[1] > lims[2]) {
    trans <- "reverse"
  } else {
    trans <- "identity"
  }

  make_a_scale("continuous", var, limits = lims, trans = trans)
}

make_a_scale <- function(type, var, ...) {
  a_scale <- match.fun(paste("a_scale_", var, "_", type, sep = ""))
  a_scale(...)
}

#' @export
limits.character <- function(lims, var) {
  make_a_scale("discrete", var, limits = lims)
}
#' @export
limits.factor <- function(lims, var) {
  make_a_scale("discrete", var, limits = as.character(lims))
}
#' @export
limits.Date <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_a_scale("date", var, limits = lims)
}
#' @export
limits.POSIXct <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_a_scale("datetime", var, limits = lims)
}
#' @export
limits.POSIXlt <- function(lims, var) {
  stopifnot(length(lims) == 2)
  make_a_scale("datetime", var, limits = as.POSIXct(lims))
}

#' Expand the plot limits with data.
#'
#. Sometimes you may want to ensure limits include a single value, for all
#' panels or all plots.  This function is a thin wrapper around
#' \code{\link{a_geom_blank}} that makes it easy to add such values.
#'
#' @param ... named list of aesthetics specifying the value (or values) that
#'   should be included in each scale.
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point()
#' p + expand_limits(x = 0)
#' p + expand_limits(y = c(1, 9))
#' p + expand_limits(x = 0, y = 0)
#'
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(colour = cyl)) +
#'   expand_limits(colour = seq(2, 10, by = 2))
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(colour = factor(cyl))) +
#'   expand_limits(colour = factor(seq(2, 10, by = 2)))
expand_limits <- function(...) {
  data <- data.frame(..., stringsAsFactors = FALSE)

  a_geom_blank(a_aes_all(names(data)), data, inherit.a_aes = FALSE)
}
