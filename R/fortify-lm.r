#' Supplement the data fitted to a linear model with model fit statistics.
#'
#' If you have missing values in your model data, you may need to refit
#' the model with \code{na.action = na.exclude}.
#'
#' @return The original data with extra columns:
#'   \item{.hat}{Diagonal of the hat matrix}
#'   \item{.sigma}{Estimate of residual standard deviation when
#'     corresponding observation is dropped from model}
#'   \item{.cooksd}{Cooks distance, \code{\link{cooks.distance}}}
#'   \item{.fitted}{Fitted values of model}
#'   \item{.resid}{Residuals}
#'   \item{.stdresid}{Standardised residuals}
#' @param model linear model
#' @param data data set, defaults to data used to fit model
#' @param ... not used by this method
#' @export
#' @examples
#' mod <- lm(mpg ~ wt, data = mtcars)
#' head(a_fortify(mod))
#' head(a_fortify(mod, mtcars))
#'
#' plot(mod, which = 1)
#'
#' a_plot(mod, aes(.fitted, .resid)) +
#'   geom_point() +
#'   geom_hline(yintercept = 0) +
#'   geom_smooth(se = FALSE)
#'
#' a_plot(mod, aes(.fitted, .stdresid)) +
#'   geom_point() +
#'   geom_hline(yintercept = 0) +
#'   geom_smooth(se = FALSE)
#'
#' a_plot(a_fortify(mod, mtcars), aes(.fitted, .stdresid)) +
#'   geom_point(aes(colour = factor(cyl)))
#'
#' a_plot(a_fortify(mod, mtcars), aes(mpg, .stdresid)) +
#'   geom_point(aes(colour = factor(cyl)))
#'
#' plot(mod, which = 2)
#' a_plot(mod) +
#'   stat_qq(aes(sample = .stdresid)) +
#'   geom_abline()
#'
#' plot(mod, which = 3)
#' a_plot(mod, aes(.fitted, sqrt(abs(.stdresid)))) +
#'   geom_point() +
#'   geom_smooth(se = FALSE)
#'
#' plot(mod, which = 4)
#' a_plot(mod, aes(seq_along(.cooksd), .cooksd)) +
#'   geom_bar(stat = "identity")
#'
#' plot(mod, which = 5)
#' a_plot(mod, aes(.hat, .stdresid)) +
#'   geom_vline(size = 2, colour = "white", xintercept = 0) +
#'   geom_hline(size = 2, colour = "white", yintercept = 0) +
#'   geom_point() + geom_smooth(se = FALSE)
#'
#' a_plot(mod, aes(.hat, .stdresid)) +
#'   geom_point(aes(size = .cooksd)) +
#'   geom_smooth(se = FALSE, size = 0.5)
#'
#' plot(mod, which = 6)
#' a_plot(mod, aes(.hat, .cooksd)) +
#'   geom_vline(xintercept = 0, colour = NA) +
#'   geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
#'   geom_smooth(se = FALSE) +
#'   geom_point()
#'
#' a_plot(mod, aes(.hat, .cooksd)) +
#'   geom_point(aes(size = .cooksd / .hat)) +
#'   scale_size_area()
a_fortify.lm <- function(model, data = model$model, ...) {
  infl <- stats::influence(model, do.coef = FALSE)
  data$.hat <- infl$hat
  data$.sigma <- infl$sigma
  data$.cooksd <- stats::cooks.distance(model, infl)

  data$.fitted <- stats::predict(model)
  data$.resid <- stats::resid(model)
  data$.stdresid <- stats::rstandard(model, infl)

  data
}
