#' Add a new component to a a_plot or a_theme object.
#'
#' This operator allows you to add objects to a a_plot or a_theme object.
#'
#' If the first object is an object of class \code{a_plot}, you can add
#' the following types of objects, and it will return a modified a_plot
#' object.
#'
#' \itemize{
#'   \item \code{data.frame}: replace current data.frame
#'      (must use \code{\%+\%})
#'   \item \code{uneval}: replace current aesthetics
#'   \item \code{a_layer}: add new layer
#'   \item \code{a_theme}: update plot a_theme
#'   \item \code{a_scale}: replace current scale
#'   \item \code{a_coord}: override current coordinate system
#'   \item \code{a_facet}: override current coordinate faceting
#' }
#'
#' If the first object is an object of class \code{a_theme}, you can add
#' another a_theme object. This will return a modified a_theme object.
#'
#' For a_theme objects, the \code{+} operator and the \code{\%+replace\%}
#' can be used to modify elements in a_themes.
#'
#' The \code{+} operator updates the elements of e1 that differ from
#' elements specified (not NULL) in e2.
#' Thus this operator can be used to incrementally add or modify attributes
#' of a a_plot a_theme.
#'
#' In contrast, the \code{\%+replace\%} operator replaces the
#' entire element; any element of a a_theme not specified in e2 will not be
#' present in the resulting a_theme (i.e. NULL).
#' Thus this operator can be used to overwrite an entire a_theme.
#'
#' @examples
#' ### Adding objects to a a_plot object
#' p <- a_plot(mtcars, a_aes(wt, mpg, colour = disp)) +
#'   a_geom_point()
#'
#' p
#' p + a_coord_cartesian(ylim = c(0, 40))
#' p + a_scale_colour_continuous(breaks = c(100, 300))
#' p + a_guides(colour = "colourbar")
#'
#' # Use a different data frame
#' m <- mtcars[1:10, ]
#' p %+% m
#'
#' ### Adding objects to a a_theme object
#' # Compare these results of adding a_theme objects to other a_theme objects
#' add_el <- a_theme_grey() + a_theme(text = a_element_text(family = "Times"))
#' rep_el <- a_theme_grey() %+replace% a_theme(text = a_element_text(family = "Times"))
#'
#' add_el$text
#' rep_el$text
#'
#' @param e1 An object of class \code{a_plot} or \code{a_theme}
#' @param e2 A component to add to \code{e1}
#' @export
#' @seealso \code{\link{a_theme}}
#' @method + aaa
#' @rdname aaa-add
"+.aaa" <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (is.a_theme(e1))  add_a_theme(e1, e2, e2name)
  else if (is.a_plot(e1)) add_a_plot(e1, e2, e2name)
}


#' @rdname aaa-add
#' @export
"%+%" <- `+.aaa`


add_a_plot <- function(p, object, objectname) {
  if (is.null(object)) return(p)

  p <- a_plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (is.a_theme(object)) {
    p$a_theme <- update_a_theme(p$a_theme, object)
  } else if (inherits(object, "a_Scale")) {
    p$scales$add(object)
  } else if (inherits(object, "a_labels")) {
    p <- update_a_labels(p, object)
  } else if (inherits(object, "a_guides")) {
    p <- update_a_guides(p, object)
  } else if (inherits(object, "uneval")) {
      p$mapping <- defaults(object, p$mapping)

      a_labels <- lapply(object, deparse)
      names(a_labels) <- names(object)
      p <- update_a_labels(p, a_labels)
  } else if (is.a_Coord(object)) {
      p$coordinates <- object
      p
  } else if (is.a_facet(object)) {
      p$a_facet <- object
      p
  } else if (is.list(object)) {
    for (o in object) {
      p <- p + o
    }
  } else if (is.a_layer(object)) {
    p$layers <- append(p$layers, object)

    # Add any new a_labels
    mapping <- make_labels(object$mapping)
    default <- make_labels(object$a_stat$default_aes)
    new_labels <- defaults(mapping, default)
    p$a_labels <- defaults(p$a_labels, new_labels)
  } else {
    stop("Don't know how to add ", objectname, " to a plot",
      call. = FALSE)
  }
  set_last_plot(p)
  p
}
