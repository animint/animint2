#' Theme a_element: blank.
#' This theme element draws nothing, and assigns no space
#'
#' @export
a_element_blank <- function() {
  structure(
    list(),
    class = c("a_element_blank", "a_element")
  )
}

#' Theme a_element: rectangle.
#'
#' Most often used for backgrounds and borders.
#'
#' @param fill fill colour
#' @param colour border colour
#' @param size border size
#' @param linetype border linetype
#' @param color an alias for \code{colour}
#' @export
a_element_rect <- function(fill = NULL, colour = NULL, size = NULL,
  linetype = NULL, color = NULL) {

  if (!is.null(color))  colour <- color
  structure(
    list(fill = fill, colour = colour, size = size, linetype = linetype),
    class = c("a_element_rect", "a_element")
  )
}

#' Theme a_element: line.
#'
#' @param colour line colour
#' @param size line size
#' @param linetype line type
#' @param lineend line end
#' @param color an alias for \code{colour}
#' @export
a_element_line <- function(colour = NULL, size = NULL, linetype = NULL,
  lineend = NULL, color = NULL) {

  if (!is.null(color))  colour <- color
  structure(
    list(colour = colour, size = size, linetype = linetype, lineend = lineend),
    class = c("a_element_line", "a_element")
  )
}


#' Theme a_element: text.
#'
#' @param family font family
#' @param face font face ("plain", "italic", "bold", "bold.italic")
#' @param colour text colour
#' @param size text size (in pts)
#' @param hjust horizontal justification (in [0, 1])
#' @param vjust vertical justification (in [0, 1])
#' @param angle angle (in [0, 360])
#' @param lineheight line height
#' @param color an alias for \code{colour}
#' @param margin margins around the text. See \code{\link{margin}} for more
#'   details. When creating a theme, the margins should be placed on the
#'   side of the text facing towards the center of the plot.
#' @param debug If \code{TRUE}, aids visual debugging by drawing a solid
#'   rectangle behind the complete text area, and a point where each a_label
#'   is anchored.
#' @export
a_element_text <- function(family = NULL, face = NULL, colour = NULL,
  size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  color = NULL, margin = NULL, debug = NULL) {

  if (!is.null(color))  colour <- color
  structure(
    list(family = family, face = face, colour = colour, size = size,
      hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
      margin = margin, debug = debug),
    class = c("a_element_text", "a_element")
  )
}


#' @export
print.a_element <- function(x, ...) utils::str(x)


#' Relative sizing for theme elements
#'
#' @param x A number representing the relative size
#' @examples
#' df <- data.frame(x = 1:3, y = 1:3)
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point() +
#'   a_theme(axis.title.x = a_element_text(size = rel(2.5)))
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

#' @export
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' Reports whether x is a rel object
#' @param x An object to test
is.rel <- function(x) inherits(x, "rel")

# Given a theme object and element name, return a grob for the element
a_element_render <- function(a_theme, a_element, ..., name = NULL) {

  # Get the a_element from the a_theme, calculating inheritance
  el <- calc_element(a_element, a_theme)
  if (is.null(el)) {
    message("Theme element ", a_element, " missing")
    return(a_zeroGrob())
  }

  ggname(paste(a_element, name, sep = "."), a_element_grob(el, ...))
}


# Returns NULL if x is length 0
len0_null <- function(x) {
  if (length(x) == 0)  NULL
  else                 x
}


#' Generate grid grob from theme a_element
#'
#' @param a_element Theme a_element, i.e. \code{a_element_rect} or similar.
#' @param ... Other arguments to control specific of rendering. This is
#'   usually at least position. See the source code for individual methods.
#' @keywords internal
#' @export
a_element_grob <- function(a_element, ...) {
  UseMethod("a_element_grob")
}

#' @export
a_element_grob.a_element_blank <- function(a_element, ...)  a_zeroGrob()

#' @export
a_element_grob.a_element_rect <- function(a_element, x = 0.5, y = 0.5,
  width = 1, height = 1,
  fill = NULL, colour = NULL, size = NULL, linetype = NULL, ...) {

  # The gp settings can override a_element_gp
  gp <- gpar(lwd = len0_null(size * .pt), col = colour, fill = fill, lty = linetype)
  a_element_gp <- gpar(lwd = len0_null(a_element$size * .pt), col = a_element$colour,
    fill = a_element$fill, lty = a_element$linetype)

  rectGrob(x, y, width, height, gp = utils::modifyList(a_element_gp, gp), ...)
}


#' @export
a_element_grob.a_element_text <- function(a_element, a_label = "", x = NULL, y = NULL,
  family = NULL, face = NULL, colour = NULL, size = NULL,
  hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  margin = NULL, expand_x = FALSE, expand_y = FALSE, ...) {

  if (is.null(a_label))
    return(a_zeroGrob())

  vj <- vjust %||% a_element$vjust
  hj <- hjust %||% a_element$hjust
  margin <- margin %||% a_element$margin

  angle <- angle %||% a_element$angle
  if (is.null(angle)) {
    stop("Text element requires non-NULL value for 'angle'.")
  }

  # The gp settings can override a_element_gp
  gp <- gpar(fontsize = size, col = colour,
    fontfamily = family, fontface = face,
    lineheight = lineheight)
  a_element_gp <- gpar(fontsize = a_element$size, col = a_element$colour,
    fontfamily = a_element$family, fontface = a_element$face,
    lineheight = a_element$lineheight)

  titleGrob(a_label, x, y, hjust = hj, vjust = vj, angle = angle,
    gp = utils::modifyList(a_element_gp, gp), margin = margin,
    expand_x = expand_x, expand_y = expand_y, debug = a_element$debug)
}



#' @export
a_element_grob.a_element_line <- function(a_element, x = 0:1, y = 0:1,
  colour = NULL, size = NULL, linetype = NULL, lineend = NULL,
  default.units = "npc", id.lengths = NULL, ...) {

  # The gp settings can override a_element_gp
  gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype, lineend = lineend)
  a_element_gp <- gpar(lwd = len0_null(a_element$size * .pt), col = a_element$colour,
    lty = a_element$linetype, lineend = a_element$lineend)

  polylineGrob(
    x, y, default.units = default.units,
    gp = utils::modifyList(a_element_gp, gp),
    id.lengths = id.lengths, ...
  )
}



# Define an element's class and what other elements it inherits from
#
# @param class The name of class (like "a_element_line", "a_element_text",
#  or the reserved "character", which means a character vector (not
#  "character" class)
# @param inherit A vector of strings, naming the elements that this
#  element inherits from.
el_def <- function(class = NULL, inherit = NULL, description = NULL) {
  list(class = class, inherit = inherit, description = description)
}


# This data structure represents the theme elements and the inheritance
# among them.
.a_element_tree <- list(
  line                = el_def("a_element_line"),
  rect                = el_def("a_element_rect"),
  text                = el_def("a_element_text"),
  title               = el_def("a_element_text", "text"),
  axis.line           = el_def("a_element_line", "line"),
  axis.text           = el_def("a_element_text", "text"),
  axis.title          = el_def("a_element_text", "title"),
  axis.ticks          = el_def("a_element_line", "line"),
  legend.key.size     = el_def("unit"),
  panel.grid          = el_def("a_element_line", "line"),
  panel.grid.major    = el_def("a_element_line", "panel.grid"),
  panel.grid.minor    = el_def("a_element_line", "panel.grid"),
  strip.text          = el_def("a_element_text", "text"),

  axis.line.x         = el_def("a_element_line", "axis.line"),
  axis.line.y         = el_def("a_element_line", "axis.line"),
  axis.text.x         = el_def("a_element_text", "axis.text"),
  axis.text.y         = el_def("a_element_text", "axis.text"),
  axis.ticks.length   = el_def("unit"),
  axis.ticks.x        = el_def("a_element_line", "axis.ticks"),
  axis.ticks.y        = el_def("a_element_line", "axis.ticks"),
  axis.title.x        = el_def("a_element_text", "axis.title"),
  axis.title.y        = el_def("a_element_text", "axis.title"),

  legend.background   = el_def("a_element_rect", "rect"),
  legend.margin       = el_def("unit"),
  legend.key          = el_def("a_element_rect", "rect"),
  legend.key.height   = el_def("unit", "legend.key.size"),
  legend.key.width    = el_def("unit", "legend.key.size"),
  legend.text         = el_def("a_element_text", "text"),
  legend.text.align   = el_def("character"),
  legend.title        = el_def("a_element_text", "title"),
  legend.title.align  = el_def("character"),
  legend.a_position     = el_def("character"),  # Need to also accept numbers
  legend.direction    = el_def("character"),
  legend.justification = el_def("character"),
  legend.box          = el_def("character"),
  legend.box.just     = el_def("character"),

  panel.background    = el_def("a_element_rect", "rect"),
  panel.border        = el_def("a_element_rect", "rect"),
  panel.margin        = el_def("unit"),
  panel.margin.x      = el_def("unit", "panel.margin"),
  panel.margin.y      = el_def("unit", "panel.margin"),
  panel.grid.major.x  = el_def("a_element_line", "panel.grid.major"),
  panel.grid.major.y  = el_def("a_element_line", "panel.grid.major"),
  panel.grid.minor.x  = el_def("a_element_line", "panel.grid.minor"),
  panel.grid.minor.y  = el_def("a_element_line", "panel.grid.minor"),
  panel.ontop         = el_def("logical"),

  strip.background    = el_def("a_element_rect", "rect"),
  strip.text.x        = el_def("a_element_text", "strip.text"),
  strip.text.y        = el_def("a_element_text", "strip.text"),
  strip.switch.pad.grid = el_def("unit"),
  strip.switch.pad.wrap = el_def("unit"),

  plot.background     = el_def("a_element_rect", "rect"),
  plot.title          = el_def("a_element_text", "title"),
  plot.subtitle       = el_def("a_element_text", "title"),
  plot.caption        = el_def("a_element_text", "title"),
  plot.margin         = el_def("margin"),

  aspect.ratio        = el_def("character")
)


# Check that an element object has the proper class
#
# Given an element object and the name of the element, this function
# checks it against the element inheritance tree to make sure the
# element is of the correct class
#
# It throws error if invalid, and returns invisible() if valid.
#
# @param el an element
# @param elname the name of the element
validate_element <- function(el, elname) {
  eldef <- .a_element_tree[[elname]]

  if (is.null(eldef)) {
    stop('"', elname, '" is not a valid theme element name.')
  }

  # NULL values for elements are OK
  if (is.null(el)) return()

  if (eldef$class == "character") {
    # Need to be a bit looser here since sometimes it's a string like "top"
    # but sometimes its a vector like c(0,0)
    if (!is.character(el) && !is.numeric(el))
      stop("Element ", elname, " must be a string or numeric vector.")
  } else if (eldef$class == "margin") {
    if (!is.unit(el) && length(el) == 4)
      stop("Element ", elname, " must be a unit vector of length 4.")
  } else if (!inherits(el, eldef$class) && !inherits(el, "a_element_blank")) {
      stop("Element ", elname, " must be a ", eldef$class, " object.")
  }
  invisible()
}
