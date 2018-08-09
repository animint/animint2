#' Fortify methods for objects produced by \pkg{multcomp}
#'
#' @param model an object of class \code{glht}, \code{confint.glht},
#'  \code{summary.glht} or \code{\link[multcomp]{cld}}
#' @param data,... other arguments to the generic ignored in this method.
#' @name a_fortify-multcomp
#' @examples
#' if (require("multcomp")) {
#' amod <- aov(breaks ~ wool + tension, data = warpbreaks)
#' wht <- glht(amod, linfct = mcp(tension = "Tukey"))
#'
#' a_fortify(wht)
#' a_plot(wht, a_aes(lhs, estimate)) + a_geom_point()
#'
#' CI <- confint(wht)
#' a_fortify(CI)
#' a_plot(CI, a_aes(lhs, estimate, ymin = lwr, ymax = upr)) +
#'    a_geom_pointrange()
#'
#' a_fortify(summary(wht))
#' a_plot(mapping = a_aes(lhs, estimate)) +
#'    a_geom_linerange(a_aes(ymin = lwr, ymax = upr), data = CI) +
#'    a_geom_point(a_aes(size = p), data = summary(wht)) +
#'    a_scale_size(trans = "reverse")
#'
#' cld <- cld(wht)
#' a_fortify(cld)
#' }
NULL

#' @method a_fortify glht
#' @rdname a_fortify-multcomp
#' @export
a_fortify.glht <- function(model, data, ...) {
  plyr::unrowname(data.frame(
    lhs = rownames(model$linfct),
    rhs = model$rhs,
    estimate = stats::coef(model),
    check.names = FALSE,
    stringsAsFactors = FALSE))
}

#' @rdname a_fortify-multcomp
#' @method a_fortify confint.glht
#' @export
a_fortify.confint.glht <- function(model, data, ...) {
  coef <- model$confint
  colnames(coef) <- tolower(colnames(coef))

  plyr::unrowname(data.frame(
    lhs = rownames(coef),
    rhs = model$rhs,
    coef,
    check.names = FALSE,
    stringsAsFactors = FALSE))
}

#' @method a_fortify summary.glht
#' @rdname a_fortify-multcomp
#' @export
a_fortify.summary.glht <- function(model, data, ...) {
  coef <- as.data.frame(
    model$test[c("coefficients", "sigma", "tstat", "pvalues")])
  names(coef) <- c("estimate", "se", "t", "p")

  plyr::unrowname(data.frame(
    lhs = rownames(coef),
    rhs = model$rhs,
    coef,
    check.names = FALSE,
    stringsAsFactors = FALSE))
}


#' @method a_fortify cld
#' @rdname a_fortify-multcomp
#' @export
a_fortify.cld <- function(model, data, ...) {
  plyr::unrowname(data.frame(
    lhs = names(model$mcletters$Letters),
    letters = model$mcletters$Letters,
    check.names = FALSE,
    stringsAsFactors = FALSE))
}
