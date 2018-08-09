#' @param quantiles conditional quantiles of y to calculate and display
#' @param formula formula relating y variables to x variables
#' @param method Quantile regression method to use.  Currently only supports
#'    \code{\link[quantreg]{rq}}.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @section Computed variables:
#' \describe{
#'   \item{quantile}{quantile of distribution}
#' }
#' @export
#' @rdname a_geom_quantile
a_stat_quantile <- function(mapping = NULL, data = NULL,
                          a_geom = "quantile", a_position = "identity",
                          ...,
                          quantiles = c(0.25, 0.5, 0.75),
                          formula = NULL,
                          method = "rq",
                          method.args = list(),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatQuantile,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      quantiles = quantiles,
      formula = formula,
      method = method,
      method.args = method.args,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatQuantile <- a_ggproto("a_StatQuantile", a_Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, quantiles = c(0.25, 0.5, 0.75),
                           formula = NULL, xseq = NULL, method = "rq",
                           method.args = list(), lambda = 1, na.rm = FALSE) {
    try_require("quantreg", "a_stat_quantile")

    if (is.null(formula)) {
      if (method == "rqss") {
        try_require("MatrixModels", "a_stat_quantile")
        formula <- eval(substitute(y ~ qss(x, lambda = lambda)),
          list(lambda = lambda))
      } else {
        formula <- y ~ x
      }
      message("Smoothing formula not specified. Using: ",
        deparse(formula))
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      xmin <- min(data$x, na.rm = TRUE)
      xmax <- max(data$x, na.rm = TRUE)
      xseq <- seq(xmin, xmax, length.out = 100)
    }
    grid <- data.frame(x = xseq)

    method <- match.fun(method)

    plyr::ldply(quantiles, quant_pred, data = data, method = method,
      formula = formula, weight = weight, grid = grid, method.args = method.args)
  }
)

quant_pred <- function(quantile, data, method, formula, weight, grid,
                       method.args = method.args) {
  args <- c(list(quote(formula), data = quote(data), tau = quote(quantile),
    weights = quote(weight)), method.args)
  model <- do.call(method, args)

  grid$y <- stats::predict(model, newdata = grid)
  grid$quantile <- quantile
  grid$group <- paste(data$group[1], quantile, sep = "-")

  grid
}
