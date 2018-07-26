#' @export
#' @rdname a_position_stack
a_position_fill <- function() {
  a_PositionFill
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_PositionFill <- a_ggproto("a_PositionFill", a_Position,
  required_aes = c("x", "ymax"),

  setup_data = function(self, data, params) {
    if (!is.null(data$ymin) && !all(data$ymin == 0))
      warning("Filling not well defined when ymin != 0", call. = FALSE)

    a_ggproto_parent(a_Position, self)$setup_data(data)
  },

  compute_panel = function(data, params, scales) {
    collide(data, NULL, "a_position_fill", pos_fill)
  }
)
