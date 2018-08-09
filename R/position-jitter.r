#' Jitter points to avoid overplotting.
#'
#' @family position adjustments
#' @param width,height Amount of vertical and horizontal jitter. The jitter
#'   is added in both positive and negative directions, so the total spread
#'   is twice the value specified here.
#'
#'   If omitted, defaults to 40\% of the resolution of the data: this means the
#'   jitter values will occupy 80\% of the implied bins. Categorical data
#'   is aligned on the integers, so a width or height of 0.5 will spread the
#'   data so it's not possible to see the distinction between the categories.
#' @export
#' @examples
#' a_plot(mtcars, a_aes(am, vs)) + a_geom_point()
#'
#' # Default amount of jittering will generally be too much for
#' # small datasets:
#' a_plot(mtcars, a_aes(am, vs)) + a_geom_jitter()
#'
#' # Two ways to override
#' a_plot(mtcars, a_aes(am, vs)) +
#'   a_geom_jitter(width = 0.1, height = 0.1)
#' a_plot(mtcars, a_aes(am, vs)) +
#'   a_geom_jitter(a_position = a_position_jitter(width = 0.1, height = 0.1))
#'
#' # The default works better for large datasets, where it will
#' # take up as much space as a boxplot or a bar
#' a_plot(mpg, a_aes(class, hwy)) +
#'   a_geom_jitter() +
#'   a_geom_boxplot()
a_position_jitter <- function(width = NULL, height = NULL) {
  a_ggproto(NULL, a_PositionJitter,
    width = width,
    height = height
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_PositionJitter <- a_ggproto("a_PositionJitter", a_Position,
  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(
      width = self$width %||% resolution(data$x, zero = FALSE) * 0.4,
      height = self$height %||% resolution(data$y, zero = FALSE) * 0.4
    )
  },

  compute_layer = function(data, params, panel) {
    trans_x <- if (params$width > 0) function(x) jitter(x, amount = params$width)
    trans_y <- if (params$height > 0) function(x) jitter(x, amount = params$height)

    transform_position(data, trans_x, trans_y)
  }
)
