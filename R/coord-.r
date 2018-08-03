#' @section a_Coordinate systems:
#'
#' All \code{a_coord_*} functions (like \code{a_coord_trans}) return a \code{a_Coord*}
#' object (like \code{a_CoordTrans}). The \code{a_Coord*} object is responsible for
#' adjusting the position of overlapping geoms.
#'
#' The way that the \code{a_coord_*} functions work is slightly different from the
#' \code{a_geom_*} and \code{a_stat_*} functions, because a \code{a_coord_*} function
#' actually "instantiates" the \code{a_Coord*} object by creating a descendant,
#' and returns that.
#'
#' Each of the \code{a_Coord*} objects is a \code{\link{a_ggproto}} object,
#' descended from the top-level \code{a_Coord}.  To create a new type of Coord
#' object, you typically will want to implement one or more of the following:
#'
#' \itemize{
#'   \item \code{aspect}: Returns the desired aspect ratio for the plot.
#'   \item \code{a_labels}: Returns a list containing labels for x and y.
#'   \item \code{render_fg}: Renders foreground elements.
#'   \item \code{render_bg}: Renders background elements.
#'   \item \code{render_axis_h}: Renders the horizontal axis.
#'   \item \code{render_axis_v}: Renders the vertical axis.
#'   \item \code{range}: Returns the x and y ranges
#'   \item \code{train}: Return the trained scale ranges.
#'   \item \code{transform}: Transforms x and y coordinates.
#'   \item \code{distance}: Calculates distance.
#'   \item \code{is_linear}: Returns \code{TRUE} if the coordinate system is
#'     linear; \code{FALSE} otherwise.
#' }
#'
#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_Coord <- a_ggproto("a_Coord",

  aspect = function(ranges) NULL,

  a_labels = function(scale_details) scale_details,

  render_fg = function(scale_details, a_theme) a_element_render(a_theme, "panel.border"),

  render_bg = function(scale_details, a_theme) {
    x.major <- if (length(scale_details$x.major) > 0) unit(scale_details$x.major, "native")
    x.minor <- if (length(scale_details$x.minor) > 0) unit(scale_details$x.minor, "native")
    y.major <- if (length(scale_details$y.major) > 0) unit(scale_details$y.major, "native")
    y.minor <- if (length(scale_details$y.minor) > 0) unit(scale_details$y.minor, "native")

    a_guide_grid(a_theme, x.minor, x.major, y.minor, y.major)
  },

  render_axis_h = function(scale_details, a_theme) {
    a_guide_axis(scale_details$x.major, scale_details$x.a_labels, "bottom", a_theme)
  },

  render_axis_v = function(scale_details, a_theme) {
    a_guide_axis(scale_details$y.major, scale_details$y.a_labels, "left", a_theme)
  },

  range = function(scale_details) {
    return(list(x = scale_details$x.range, y = scale_details$y.range))
  },

  train = function(scale_details) NULL,

  transform = function(data, range) NULL,

  distance = function(x, y, scale_details) NULL,

  is_linear = function() FALSE
)

#' Is this object a coordinate system?
#'
#' @export is.a_Coord
#' @keywords internal
is.a_Coord <- function(x) inherits(x, "a_Coord")

expand_default <- function(a_scale, discrete = c(0, 0.6), continuous = c(0.05, 0)) {
  a_scale$expand %|W|% if (a_scale$is_discrete()) discrete else continuous
}
