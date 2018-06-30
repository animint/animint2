#' Mutable ranges have a two methods (\code{train} and \code{reset}), and make
#' it possible to build up complete ranges with multiple passes.
#'
#' These range objects should be instantiated with
#' \code{\link{continuous_range}} and \code{\link{discrete_range}}.
#'
#' @noRd
a_Range <- a_ggproto("a_Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

a_RangeDiscrete <- a_ggproto("a_RangeDiscrete", a_Range,
  train = function(self, x, drop = FALSE) {
    self$range <- scales::train_discrete(x, self$range, drop)
  }
)

a_RangeContinuous <- a_ggproto("a_RangeContinuous", a_Range,
  train = function(self, x) {
    self$range <- scales::train_continuous(x, self$range)
  }
)

continuous_range <- function() {
  a_ggproto(NULL, a_RangeContinuous)
}

discrete_range <- function() {
  a_ggproto(NULL, a_RangeDiscrete)
}
