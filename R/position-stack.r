#' Stack overlapping objects on top of one another.
#'
#' \code{a_position_fill} additionally standardises each stack to have unit
#' height.
#'
#' @family position adjustments
#' @seealso See \code{\link{a_geom_bar}} and \code{\link{a_geom_area}} for
#'   more examples.
#' @export
#' @examples
#' # Stacking is the default behaviour for most area plots:
#' a_plot(mtcars, a_aes(factor(cyl), fill = factor(vs))) + a_geom_bar()
#' # Fill makes it easier to compare proportions
#' a_plot(mtcars, a_aes(factor(cyl), fill = factor(vs))) +
#'   a_geom_bar(a_position = "fill")
#'
#' # To change stacking order, use factor() to change order of levels
#' mtcars$vs <- factor(mtcars$vs, levels = c(1,0))
#' a_plot(mtcars, a_aes(factor(cyl), fill = factor(vs))) + a_geom_bar()
#'
#' a_plot(diamonds, a_aes(price, fill = cut)) +
#'   a_geom_histogram(binwidth = 500)
#' # When used with a histogram, a_position_fill creates a conditional density
#' # estimate
#' a_plot(diamonds, a_aes(price, fill = cut)) +
#'   a_geom_histogram(binwidth = 500, a_position = "fill")
#'
#' # Stacking is also useful for time series
#' data.set <- data.frame(
#'   Time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
#'   Type = rep(c('a', 'b', 'c', 'd'), 4),
#'   Value = rpois(16, 10)
#' )
#'
#' a_plot(data.set, a_aes(Time, Value)) + a_geom_area(a_aes(fill = Type))
#'
#' # If you want to stack lines, you need to say so:
#' a_plot(data.set, a_aes(Time, Value)) + a_geom_line(a_aes(colour = Type))
#' a_plot(data.set, a_aes(Time, Value)) +
#'   a_geom_line(a_position = "stack", a_aes(colour = Type))
#'
#' # But realise that this makes it *much* harder to compare individual
#' # trends
a_position_stack <- function() {
  a_PositionStack
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_PositionStack <- a_ggproto("a_PositionStack", a_Position,
  # requires one of c("ymax", "y"),

  setup_data = function(self, data, params) {
    data = remove_missing(data, FALSE,
      c("x", "y", "ymin", "ymax", "xmin", "xmax"), name = "a_position_stack")

    if (is.null(data$ymax) && is.null(data$y)) {
      message("Missing y and ymax in a_position = 'stack'. ",
        "Maybe you want a_position = 'identity'?")
      return(data)
    }

    if (!is.null(data$ymin) && !all(data$ymin == 0))
      warning("Stacking not well defined when ymin != 0", call. = FALSE)

    data
  },

  compute_panel = function(data, params, scales) {
    collide(data, NULL, "a_position_stack", pos_stack)
  }
)
