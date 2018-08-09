#' Adjust position by dodging overlaps to the side.
#'
#' @inheritParams position_identity
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples for a use case.
#' @family position adjustments
#' @export
#' @examples
#' a_plot(mtcars, a_aes(factor(cyl), fill = factor(vs))) +
#'   a_geom_bar(a_position = "dodge")
#' \donttest{
#' a_plot(diamonds, a_aes(price, fill = cut)) +
#'   a_geom_histogram(a_position="dodge")
#' # see ?a_geom_boxplot and ?a_geom_bar for more examples
#'
#' # To dodge items with different widths, you need to be explicit
#' df <- data.frame(x = c("a","a","b","b"), y = 2:5, g = rep(1:2, 2))
#' p <- a_plot(df, a_aes(x, y, group = g)) +
#'   a_geom_bar(
#'     a_stat = "identity", a_position = "dodge",
#'     fill = "grey50", colour = "black"
#'   )
#' p
#'
#' # A line range has no width:
#' p + a_geom_linerange(a_aes(ymin = y-1, ymax = y+1), a_position = "dodge")
#' # You need to explicitly specify the width for dodging
#' p + a_geom_linerange(a_aes(ymin = y-1, ymax = y+1),
#'   a_position = a_position_dodge(width = 0.9))
#'
#' # Similarly with error bars:
#' p + a_geom_errorbar(a_aes(ymin = y-1, ymax = y+1), width = 0.2,
#'   a_position = "dodge")
#' p + a_geom_errorbar(a_aes(ymin = y-1, ymax = y+1, width = 0.2),
#'   a_position = a_position_dodge(width = 0.90))
#' }
a_position_dodge <- function(width = NULL) {
  a_ggproto(NULL, a_PositionDodge, width = width)
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_PositionDodge <- a_ggproto("a_PositionDodge", a_Position,
  required_aes = "x",
  width = NULL,
  setup_params = function(self, data) {
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warning("Width not defined. Set with `a_position_dodge(width = ?)`",
        call. = FALSE)
    }
    list(width = self$width)
  },

  compute_panel = function(data, params, scales) {
    collide(data, params$width, "a_position_dodge", pos_dodge, check.width = FALSE)
  }
)
