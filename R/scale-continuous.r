#' Continuous position scales (x & y).
#'
#' \code{a_scale_x_continuous} and \code{a_scale_y_continuous} are the key functions.
#' The others, \code{a_scale_x_log10}, \code{a_scale_y_sqrt} etc, are aliases
#' that set the \code{trans} argument to commonly used transformations.
#'
#' @inheritParams continuous_a_scale
#' @seealso \code{\link{a_scale_date}} for date/time position scales.
#' @param ... Other arguments passed on to \code{a_scale_(x|y)_continuous}
#' @examples
#' \donttest{
#' if (require(ggplot2movies)) {
#' m <- a_plot(subset(movies, votes > 1000), a_aes(rating, votes)) +
#'   a_geom_point(na.rm = TRUE)
#' m
#'
#' # Manipulating the default position scales lets you:
#'
#' #  * change the axis labels
#' m + a_scale_y_continuous("number of votes")
#' m + a_scale_y_continuous(quote(votes ^ alpha))
#'
#' #  * modify the axis limits
#' m + a_scale_y_continuous(limits = c(0, 5000))
#' m + a_scale_y_continuous(limits = c(1000, 10000))
#' m + a_scale_x_continuous(limits = c(7, 8))
#'
#' # you can also use the short hand functions xlim and ylim
#' m + ylim(0, 5000)
#' m + ylim(1000, 10000)
#' m + xlim(7, 8)
#'
#' #  * choose where the ticks appear
#' m + a_scale_x_continuous(breaks = 1:10)
#' m + a_scale_x_continuous(breaks = c(1,3,7,9))
#'
#' #  * manually label the ticks
#' m + a_scale_x_continuous(breaks = c(2,5,8), a_labels = c("two", "five", "eight"))
#' m + a_scale_x_continuous(breaks = c(2,5,8), a_labels = c("horrible", "ok", "awesome"))
#' m + a_scale_x_continuous(breaks = c(2,5,8), a_labels = expression(Alpha, Beta, Omega))
#'
#' # There are a few built in transformation that you can use:
#' m + a_scale_y_log10()
#' m + a_scale_y_sqrt()
#' m + a_scale_y_reverse()
#' # You can also create your own and supply them to the trans argument.
#' # See ?scales::trans_new
#'
#' # You can control the formatting of the labels with the formatter
#' # argument.  Some common formats are built into the scales package:
#' df <- data.frame(
#'   x = rnorm(10) * 100000,
#'   y = seq(0, 1, length.out = 10)
#' )
#' p <- a_plot(df, a_aes(x, y)) + a_geom_point()
#' p + a_scale_y_continuous(a_labels = scales::percent)
#' p + a_scale_y_continuous(a_labels = scales::dollar)
#' p + a_scale_x_continuous(a_labels = scales::comma)
#'
#' # Other shortcut functions
#' a_plot(movies, a_aes(rating, votes)) +
#'   a_geom_point() +
#'   ylim(1e4, 5e4)
#' #   * axis labels
#' a_plot(movies, a_aes(rating, votes)) +
#'   a_geom_point() +
#'   labs(x = "My x axis", y = "My y axis")
#' #   * log scaling
#' a_plot(movies, a_aes(rating, votes)) +
#'   a_geom_point() +
#'   a_scale_x_log10() +
#'   a_scale_y_log10()
#' }
#' }
#' @name a_scale_continuous
NULL

#' @rdname a_scale_continuous
#' @export
a_scale_x_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), a_labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity") {
  sc <- continuous_a_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, a_labels = a_labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    a_guide = "none"
  )

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleContinuousPosition
  class(sc) <- class(a_ScaleContinuousPosition)

  sc
}

#' @rdname a_scale_continuous
#' @export
a_scale_y_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), a_labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity") {
  sc <- continuous_a_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, a_labels = a_labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    a_guide = "none"
  )

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleContinuousPosition
  class(sc) <- class(a_ScaleContinuousPosition)

  sc
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_ScaleContinuousPosition <- a_ggproto("a_ScaleContinuousPosition", a_ScaleContinuous,
                                   # Position aesthetics don't map, because the coordinate system takes
                                   # care of it. But they do need to be made in to doubles, so stat methods
                                   # can tell the difference between continuous and discrete data.
                                   map = function(self, x, limits = self$get_limits()) {
                                     scaled <- as.numeric(self$oob(x, limits))
                                     ifelse(!is.na(scaled), scaled, self$na.value)
                                   }
)

# Transformed scales ---------------------------------------------------------

#' @rdname a_scale_continuous
#' @export
a_scale_x_log10 <- function(...) {
  a_scale_x_continuous(..., trans = log10_trans())
}
#' @rdname a_scale_continuous
#' @export
a_scale_y_log10 <- function(...) {
  a_scale_y_continuous(..., trans = log10_trans())
}
#' @rdname a_scale_continuous
#' @export
a_scale_x_reverse <- function(...) {
  a_scale_x_continuous(..., trans = reverse_trans())
}
#' @rdname a_scale_continuous
#' @export
a_scale_y_reverse <- function(...) {
  a_scale_y_continuous(..., trans = reverse_trans())
}
#' @rdname a_scale_continuous
#' @export
a_scale_x_sqrt <- function(...) {
  a_scale_x_continuous(..., trans = sqrt_trans())
}
#' @rdname a_scale_continuous
#' @export
a_scale_y_sqrt <- function(...) {
  a_scale_y_continuous(..., trans = sqrt_trans())
}
