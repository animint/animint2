#' Superimpose a function.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_stat", "function")}
#'
#' @param fun function to use
#' @param n number of points to interpolate along
#' @param args list of additional arguments to pass to \code{fun}
#' @param xlim Optionally, restrict the range of the function to this range.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @section Computed variables:
#' \describe{
#'   \item{x}{x's along a grid}
#'   \item{y}{value of function evaluated at corresponding x}
#' }
#' @export
#' @examples
#' set.seed(1492)
#' df <- data.frame(
#'   x = rnorm(100)
#' )
#' x <- df$x
#' base <- a_plot(df, a_aes(x)) + a_geom_density()
#' base + a_stat_function(fun = dnorm, colour = "red")
#' base + a_stat_function(fun = dnorm, colour = "red", args = list(mean = 3))
#'
#' # Plot functions without data
#' # Examples adapted from Kohske Takahashi
#'
#' # Specify range of x-axis
#' a_plot(data.frame(x = c(0, 2)), a_aes(x)) +
#'   a_stat_function(fun = exp, a_geom = "line")
#'
#' # Plot a normal curve
#' a_plot(data.frame(x = c(-5, 5)), a_aes(x)) + a_stat_function(fun = dnorm)
#'
#' # To specify a different mean or sd, use the args parameter to supply new values
#' a_plot(data.frame(x = c(-5, 5)), a_aes(x)) +
#'   a_stat_function(fun = dnorm, args = list(mean = 2, sd = .5))
#'
#' # Two functions on the same plot
#' f <- a_plot(data.frame(x = c(0, 10)), a_aes(x))
#' f + a_stat_function(fun = sin, colour = "red") +
#'   a_stat_function(fun = cos, colour = "blue")
#'
#' # Using a custom function
#' test <- function(x) {x ^ 2 + x + 20}
#' f + a_stat_function(fun = test)
a_stat_function <- function(mapping = NULL, data = NULL,
                          a_geom = "path", a_position = "identity",
                          ...,
                          fun,
                          xlim = NULL,
                          n = 101,
                          args = list(),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatFunction,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      fun = fun,
      n = n,
      args = args,
      na.rm = na.rm,
      xlim = xlim,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatFunction <- a_ggproto("a_StatFunction", a_Stat,
  default_aes = a_aes(y = ..y..),

  compute_group = function(data, scales, fun, xlim = NULL, n = 101, args = list()) {
    range <- xlim %||% scales$x$dimension()
    xseq <- seq(range[1], range[2], length.out = n)

    if (scales$x$is_discrete()) {
      x_trans <- xseq
    } else {
      # For continuous scales, need to back transform from transformed range
      # to original values
      x_trans <- scales$x$trans$inverse(xseq)
    }

    data.frame(
      x = xseq,
      y = do.call(fun, c(list(quote(x_trans)), args))
    )
  }
)
