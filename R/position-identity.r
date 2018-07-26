#' Don't adjust position
#'
#' @family position adjustments
#' @export
a_position_identity <- function() {
  a_PositionIdentity
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_PositionIdentity <- a_ggproto("a_PositionIdentity", a_Position,
  compute_layer = function(data, params, scales) {
    data
  }
)
