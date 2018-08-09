#' Discrete position.
#'
#' You can use continuous positions even with a discrete position scale -
#' this allows you (e.g.) to place labels between bars in a bar chart.
#' Continuous positions are numeric values starting at one for the first
#' level, and increasing by one for each level (i.e. the labels are placed
#' at integer positions).  This is what allows jittering to work.
#'
#' @param ... common discrete scale parameters: \code{name}, \code{breaks},
#'  \code{a_labels}, \code{na.value}, \code{limits} and \code{a_guide}.  See
#'  \code{\link{discrete_a_scale}} for more details
#' @param expand a numeric vector of length two giving multiplicative and
#'   additive expansion constants. These constants ensure that the data is
#'   placed some distance away from the axes.
#' @rdname a_scale_discrete
#' @export
#' @examples
#' a_plot(diamonds, a_aes(cut)) + a_geom_bar()
#'
#' \donttest{
#' # The discrete position scale is added automatically whenever you
#' # have a discrete position.
#'
#' (d <- a_plot(subset(diamonds, carat > 1), a_aes(cut, clarity)) +
#'       a_geom_jitter())
#'
#' d + a_scale_x_discrete("Cut")
#' d + a_scale_x_discrete("Cut", a_labels = c("Fair" = "F","Good" = "G",
#'   "Very Good" = "VG","Perfect" = "P","Ideal" = "I"))
#'
#' # Use limits to adjust the which levels (and in what order)
#' # are displayed
#' d + a_scale_x_discrete(limits = c("Fair","Ideal"))
#'
#' # you can also use the short hand functions xlim and ylim
#' d + xlim("Fair","Ideal", "Good")
#' d + ylim("I1", "IF")
#'
#' # See ?reorder to reorder based on the values of another variable
#' a_plot(mpg, a_aes(manufacturer, cty)) + a_geom_point()
#' a_plot(mpg, a_aes(reorder(manufacturer, cty), cty)) + a_geom_point()
#' a_plot(mpg, a_aes(reorder(manufacturer, displ), cty)) + a_geom_point()
#'
#' # Use abbreviate as a formatter to reduce long names
#' a_plot(mpg, a_aes(reorder(manufacturer, displ), cty)) +
#'   a_geom_point() +
#'   a_scale_x_discrete(a_labels = abbreviate)
#' }
a_scale_x_discrete <- function(..., expand = waiver()) {
  sc <- discrete_a_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                       expand = expand, a_guide = "none")

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleDiscretePosition
  class(sc) <- class(a_ScaleDiscretePosition)

  sc$range_c <- continuous_range()
  sc
}
#' @rdname a_scale_discrete
#' @export
a_scale_y_discrete <- function(..., expand = waiver()) {
  sc <- discrete_a_scale(c("y", "ymin", "ymax", "yend"), "position_d", identity, ...,
                       expand = expand, a_guide = "none")

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleDiscretePosition
  class(sc) <- class(a_ScaleDiscretePosition)

  sc$range_c <- continuous_range()
  sc
}

# The discrete position scale maintains two separate ranges - one for
# continuous data and one for discrete data.  This complicates training and
# mapping, but makes it possible to place objects at non-integer positions,
# as is necessary for jittering etc.

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_ScaleDiscretePosition <- a_ggproto("a_ScaleDiscretePosition", a_ScaleDiscrete,

                                 train = function(self, x) {
                                   if (is.discrete(x)) {
                                     self$range$train(x, drop = self$drop)
                                   } else {
                                     self$range_c$train(x)
                                   }
                                 },

                                 get_limits = function(self) {
                                   if (self$is_empty()) return(c(0, 1))
                                   self$limits %||% self$range$range %||% integer()
                                 },

                                 is_empty = function(self) {
                                   is.null(self$range$range) && is.null(self$limits) && is.null(self$range_c$range)
                                 },

                                 reset = function(self) {
                                   # Can't reset discrete scale because no way to recover values
                                   self$range_c$reset()
                                 },

                                 map = function(self, x, limits = self$get_limits()) {
                                   if (is.discrete(x)) {
                                     seq_along(limits)[match(as.character(x), limits)]
                                   } else {
                                     x
                                   }
                                 },

                                 dimension = function(self, expand = c(0, 0)) {
                                   c_range <- self$range_c$range
                                   d_range <- self$range$range

                                   if (self$is_empty()) {
                                     c(0, 1)
                                   } else if (is.null(d_range)) { # only continuous
                                     expand_range(c_range, expand[1], 0 , 1)
                                   } else if (is.null(c_range)) { # only discrete
                                     expand_range(c(1, length(d_range)), 0, expand[2], 1)
                                   } else { # both
                                     range(
                                       expand_range(c_range, expand[1], 0 , 1),
                                       expand_range(c(1, length(d_range)), 0, expand[2], 1)
                                     )
                                   }
                                 },

                                 clone = function(self) {
                                   new <- a_ggproto(NULL, self)
                                   new$range <- discrete_range()
                                   new$range_c <- continuous_range()
                                   new
                                 }
)
