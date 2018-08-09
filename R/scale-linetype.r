#' Scale for line patterns.
#'
#' Default line types based on a set supplied by Richard Pearson,
#' University of Manchester.  Line types can not be mapped to continuous
#' values.
#'
#' @inheritParams a_scale_x_discrete
#' @param na.value The linetype to use for \code{NA} values.
#' @rdname a_scale_linetype
#' @export
#' @examples
#' base <- a_plot(economics_long, a_aes(date, value01))
#' base + a_geom_line(a_aes(group = variable))
#' base + a_geom_line(a_aes(linetype = variable))
#'
#' # See a_scale_manual for more flexibility
a_scale_linetype <- function(..., na.value = "blank") {
  discrete_a_scale("linetype", "linetype_d", linetype_pal(),
    na.value = na.value, ...)
}

#' @rdname a_scale_linetype
#' @export
a_scale_linetype_continuous <- function(...) {
  stop("A continuous variable can not be mapped to linetype", call. = FALSE)
}
#' @rdname a_scale_linetype
#' @export
a_scale_linetype_discrete <- a_scale_linetype
