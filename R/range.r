#' Mutable ranges have a two methods (\code{train} and \code{reset}), and make
#' it possible to build up complete ranges with multiple passes.
#'
#' These range objects should be instantiated with
#' \code{\link{continuous_range}} and \code{\link{discrete_range}}.
#'
#' @noRd
Range <- gganimintproto("Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

RangeDiscrete <- gganimintproto("RangeDiscrete", Range,
  train = function(self, x, drop = FALSE) {
    self$range <- scales::train_discrete(x, self$range, drop)
  }
)

RangeContinuous <- gganimintproto("RangeContinuous", Range,
  train = function(self, x) {
    self$range <- scales::train_continuous(x, self$range)
  }
)

continuous_range <- function() {
  gganimintproto(NULL, RangeContinuous)
}

discrete_range <- function() {
  gganimintproto(NULL, RangeDiscrete)
}
