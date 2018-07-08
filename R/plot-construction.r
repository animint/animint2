#' Add a new component to a a_plot or theme object.
#'
#' This operator allows you to add objects to a a_plot or theme object.
#'
#' If the first object is an object of class \code{a_plot}, you can add
#' the following types of objects, and it will return a modified a_plot
#' object.
#'
#' \itemize{
#'   \item \code{data.frame}: replace current data.frame
#'      (must use \code{\%+\%})
#'   \item \code{uneval}: replace current aesthetics
#'   \item \code{layer}: add new layer
#'   \item \code{theme}: update plot theme
#'   \item \code{scale}: replace current scale
#'   \item \code{coord}: override current coordinate system
#'   \item \code{a_facet}: override current coordinate faceting
#' }
#'
#' If the first object is an object of class \code{theme}, you can add
#' another theme object. This will return a modified theme object.
#'
#' For theme objects, the \code{+} operator and the \code{\%+replace\%}
#' can be used to modify elements in themes.
#'
#' The \code{+} operator updates the elements of e1 that differ from
#' elements specified (not NULL) in e2.
#' Thus this operator can be used to incrementally add or modify attributes
#' of a a_plot theme.
#'
#' In contrast, the \code{\%+replace\%} operator replaces the
#' entire element; any element of a theme not specified in e2 will not be
#' present in the resulting theme (i.e. NULL).
#' Thus this operator can be used to overwrite an entire theme.
#'
#' @examples
#' ### Adding objects to a a_plot object
#' p <- a_plot(mtcars, aes(wt, mpg, colour = disp)) +
#'   geom_point()
#'
#' p
#' p + coord_cartesian(ylim = c(0, 40))
#' p + scale_colour_continuous(breaks = c(100, 300))
#' p + guides(colour = "colourbar")
#'
#' # Use a different data frame
#' m <- mtcars[1:10, ]
#' p %+% m
#'
#' ### Adding objects to a theme object
#' # Compare these results of adding theme objects to other theme objects
#' add_el <- theme_grey() + theme(text = a_element_text(family = "Times"))
#' rep_el <- theme_grey() %+replace% theme(text = a_element_text(family = "Times"))
#'
#' add_el$text
#' rep_el$text
#'
#' @param e1 An object of class \code{a_plot} or \code{theme}
#' @param e2 A component to add to \code{e1}
#' @export
#' @seealso \code{\link{theme}}
#' @method + aaa
#' @rdname aaa-add
"+.aaa" <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (is.theme(e1))  add_theme(e1, e2, e2name)
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
  } else if (is.theme(object)) {
    p$theme <- update_theme(p$theme, object)
  } else if (inherits(object, "a_Scale")) {
    p$scales$add(object)
  } else if (inherits(object, "labels")) {
    p <- update_labels(p, object)
  } else if (inherits(object, "guides")) {
    p <- update_guides(p, object)
  } else if (inherits(object, "uneval")) {
      p$mapping <- defaults(object, p$mapping)

      labels <- lapply(object, deparse)
      names(labels) <- names(object)
      p <- update_labels(p, labels)
  } else if (is.Coord(object)) {
      p$coordinates <- object
      p
  } else if (is.a_facet(object)) {
      p$a_facet <- object
      p
  } else if (is.list(object)) {
    for (o in object) {
      p <- p + o
    }
  } else if (is.layer(object)) {
    p$layers <- append(p$layers, object)

    # Add any new labels
    mapping <- make_labels(object$mapping)
    default <- make_labels(object$stat$default_aes)
    new_labels <- defaults(mapping, default)
    p$labels <- defaults(p$labels, new_labels)
  } else {
    stop("Don't know how to add ", objectname, " to a plot",
      call. = FALSE)
  }
  set_last_plot(p)
  p
}
