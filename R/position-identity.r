#' Don't adjust position
#'
#' @family position adjustments
#' @export
position_identity <- function() {
  PositionIdentity
}

#' @rdname animint2-gganimintproto
#' @format NULL
#' @usage NULL
#' @export
PositionIdentity <- gganimintproto("PositionIdentity", Position,
  compute_layer = function(data, params, scales) {
    data
  }
)
