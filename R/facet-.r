#' a_facet specification.
#'
#' Create new facetting specification.  For internal use only.
#'
#' @param ... object fields
#' @param shrink shrink scales to fit output of statistics, not raw data
#' @keywords internal
#' @export
a_facet <- function(..., shrink = TRUE, subclass = c()) {
  structure(list(..., shrink = shrink), class = c(subclass, "a_facet"))
}

#' Is this object a facetting specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.a_facet <- function(x) inherits(x, "a_facet")


# Figure out layout from data from plot and all layers.
#
# This creates the layout data frame which maps from data values to
# panel coordinates: ROW, COL and PANEL. It also records the panels that
# contribute to each x and y scale.
#
# @param data a list of data frames (one for the plot and one for each
#   layer)
a_facet_train_layout <- function(a_facet, data)
  UseMethod("a_facet_train_layout")

a_facet_map_layout <- function(a_facet, data, layout)
  UseMethod("a_facet_map_layout")

a_facet_render <- function(a_facet, panels_grob, a_coord, a_theme, a_geom_grobs)
  UseMethod("a_facet_render")

a_facet_strips <- function(a_facet, panel, a_theme)
  UseMethod("a_facet_strips")

a_facet_panels <- function(a_facet, panel, a_coord, a_theme, a_geom_grobs)
  UseMethod("a_facet_panels")

a_facet_axes <- function(a_facet, panel, a_coord, a_theme)
  UseMethod("a_facet_axes")

# Text description of a_facetting variables
a_facet_vars <- function(a_facet)
  UseMethod("a_facet_vars")


#' @export
format.a_facet <- function(x, ...) {
  name <- paste(rev(class(x)), collapse = "_")

  paste(name, "(", a_facet_vars(x), ")", sep = "")
}

#' @export
print.a_facet <- function(x, ...) {
  cat(format(x, ...), "\n")
}
