#' Create an annotation layer.
#'
#' This function adds geoms to a plot. Unlike typical a geom function,
#' the properties of the geoms are not mapped from variables of a data frame,
#' but are instead passed in as vectors. This is useful for adding small annotations
#' (such as text labels) or if you have your data in vectors, and for some
#' reason don't want to put them in a data frame.
#'
#' Note that all position aesthetics are scaled (i.e. they will expand the
#' limits of the plot so they are visible), but all other aesthetics are
#' set. This means that layers created with this function will never
#' affect the legend.
#'
#' @param a_geom name of a_geom to use for annotation
#' @param x,y,xmin,ymin,xmax,ymax,xend,yend positioning aesthetics -
#'   you must specify at least one of these.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(x = wt, y = mpg)) + a_geom_point()
#' p + a_annotate("text", x = 4, y = 25, a_label = "Some text")
#' p + a_annotate("text", x = 2:5, y = 25, a_label = "Some text")
#' p + a_annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,
#'   alpha = .2)
#' p + a_annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,
#'   colour = "blue")
#' p + a_annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
#'   colour = "red", size = 1.5)
#'
#' p + a_annotate("text", x = 2:3, y = 20:21, a_label = c("my label", "label 2"))
a_annotate <- function(a_geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
                     ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ...,
                     na.rm = FALSE) {

  a_position <- compact(list(
    x = x, xmin = xmin, xmax = xmax, xend = xend,
    y = y, ymin = ymin, ymax = ymax, yend = yend
  ))
  a_aesthetics <- c(a_position, list(...))

  # Check that all a_aesthetic have compatible lengths
  lengths <- vapply(a_aesthetics, length, integer(1))
  unequal <- length(unique(setdiff(lengths, 1L))) > 1L
  if (unequal) {
    bad <- lengths != 1L
    details <- paste(names(a_aesthetics)[bad], " (", lengths[bad], ")",
      sep = "", collapse = ", ")
    stop("Unequal parameter lengths: ", details, call. = FALSE)
  }

  data <- data.frame(a_position)
  a_layer(
    a_geom = a_geom,
    params = list(
      na.rm = na.rm,
      ...
    ),
    a_stat = a_StatIdentity,
    a_position = a_PositionIdentity,
    data = data,
    mapping = a_aes_all(names(data)),
    inherit.a_aes = FALSE,
    show.legend = FALSE
  )
}

