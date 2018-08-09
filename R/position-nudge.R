#' Nudge points.
#'
#' This is useful if you want to nudge labels a little ways from their
#' points.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move.
#' @export
#' @examples
#' df <- data.frame(
#'   x = c(1,3,2,5),
#'   y = c("a","c","d","c")
#' )
#'
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point() +
#'   a_geom_text(a_aes(a_label = y))
#'
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_point() +
#'   a_geom_text(a_aes(a_label = y), a_position = a_position_nudge(y = -0.1))
a_position_nudge <- function(x = 0, y = 0) {
  a_ggproto(NULL, a_PositionNudge,
    x = x,
    y = y
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_PositionNudge <- a_ggproto("a_PositionNudge", a_Position,
  x = 0,
  y = 0,

  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(x = self$x, y = self$y)
  },

  compute_layer = function(data, params, panel) {
    transform_position(data, function(x) x + params$x, function(y) y + params$y)
  }
)
