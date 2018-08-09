#' Create your own discrete scale.
#'
#' @name a_scale_manual
#' @inheritParams a_scale_x_discrete
#' @param values a set of aesthetic values to map data values to.  If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale.  Any data values that don't match will be
#'   given \code{na.value}.
#' @examples
#' \donttest{
#' p <- a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(colour = factor(cyl)))
#'
#' p + a_scale_colour_manual(values = c("red","blue", "green"))
#' p + a_scale_colour_manual(
#'   values = c("8" = "red","4" = "blue","6" = "green"))
#' # With rgb hex values
#' p + a_scale_colour_manual(values = c("#FF0000", "#0000FF", "#00FF00"))
#'
#' # As with other scales you can use breaks to control the appearance
#' # of the legend
#' cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
#' p + a_scale_colour_manual(values = cols)
#' p + a_scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
#' p + a_scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
#' p + a_scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
#' a_labels = c("four", "six", "eight"))
#'
#' # And limits to control the possible values of the scale
#' p + a_scale_colour_manual(values = cols, limits = c("4", "8"))
#' p + a_scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
#'
#' # Notice that the values are matched with limits, and not breaks
#' p + a_scale_colour_manual(limits = c(6, 8, 4), breaks = c(8, 4, 6),
#'   values = c("grey50", "grey80", "black"))
#' }
NULL

#' @rdname a_scale_manual
#' @export
a_scale_colour_manual <- function(..., values) {
  manual_a_scale("colour", values, ...)
}

#' @rdname a_scale_manual
#' @export
a_scale_fill_manual <- function(..., values) {
  manual_a_scale("fill", values, ...)
}

#' @rdname a_scale_manual
#' @export
a_scale_size_manual <- function(..., values) {
  manual_a_scale("size", values, ...)
}

#' @rdname a_scale_manual
#' @export
a_scale_shape_manual <- function(..., values) {
  manual_a_scale("shape", values, ...)
}

#' @rdname a_scale_manual
#' @export
a_scale_linetype_manual <- function(..., values) {
  manual_a_scale("linetype", values, ...)
}

#' @rdname a_scale_manual
#' @export
a_scale_alpha_manual <- function(..., values) {
  manual_a_scale("alpha", values, ...)
}


manual_a_scale <- function(a_aesthetic, values, ...) {
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n, " needed but only ",
        length(values), " provided.", call. = FALSE)
    }
    values
  }
  discrete_a_scale(a_aesthetic, "manual", pal, ...)
}
