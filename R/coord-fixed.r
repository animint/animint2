#' Cartesian coordinates with fixed relationship between x and y scales.
#'
#' A fixed scale coordinate system forces a specified ratio between the
#' physical representation of data units on the axes. The ratio represents the
#' number of units on the y-axis equivalent to one unit on the x-axis. The
#' default, \code{ratio = 1}, ensures that one unit on the x-axis is the same
#' length as one unit on the y-axis. Ratios higher than one make units on the
#' y axis longer than units on the x-axis, and vice versa. This is similar to
#' \code{\link[MASS]{eqscplot}}, but it works for all types of graphics.
#'
#' @export
#' @inheritParams a_coord_cartesian
#' @param ratio aspect ratio, expressed as \code{y / x}
#' @examples
#' # ensures that the ranges of axes are equal to the specified ratio by
#' # adjusting the plot aspect ratio
#'
#' p <- a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point()
#' p + a_coord_fixed(ratio = 1)
#' p + a_coord_fixed(ratio = 5)
#' p + a_coord_fixed(ratio = 1/5)
#'
#' # Resize the plot to see that the specified aspect ratio is maintained
a_coord_fixed <- function(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) {
  a_ggproto(NULL, a_CoordFixed,
    limits = list(x = xlim, y = ylim),
    ratio = ratio,
    expand = expand
  )
}

#' @export
#' @rdname a_coord_fixed
#' @usage NULL
a_coord_equal <- a_coord_fixed


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_CoordFixed <- a_ggproto("a_CoordFixed", a_CoordCartesian,

  aspect = function(self, ranges) {
    diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
  }
)
