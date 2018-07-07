#' The zero grob draws nothing and has zero size.
#'
#' @keywords internal
#' @export
a_zeroGrob <- function() .a_zeroGrob

.a_zeroGrob <- grob(cl = "a_zeroGrob", name = "NULL")
#' @export
#' @method widthDetails a_zeroGrob
widthDetails.a_zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method heightDetails a_zeroGrob
heightDetails.a_zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method grobWidth a_zeroGrob
grobWidth.a_zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method grobHeight a_zeroGrob
grobHeight.a_zeroGrob <- function(x) unit(0, "cm")
#' @export
#' @method drawDetails a_zeroGrob
drawDetails.a_zeroGrob <- function(x, recording) {}

is.zero <- function(x) is.null(x) || inherits(x, "a_zeroGrob")
