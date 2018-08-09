#' Display a smooth density estimate.
#'
#' A kernel density estimate, useful for display the distribution of variables
#' with underlying smoothness.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "density")}
#'
#' @seealso See \code{\link{a_geom_histogram}}, \code{\link{a_geom_freqpoly}} for
#'   other methods of displaying continuous distribution.
#'   See \code{\link{a_geom_violin}} for a compact density display.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param a_geom,a_stat Use to override the default connection between
#'   \code{a_geom_density} and \code{a_stat_density}.
#' @export
#' @examples
#' a_plot(diamonds, a_aes(carat)) +
#'   a_geom_density()
#'
#' a_plot(diamonds, a_aes(carat)) +
#'   a_geom_density(adjust = 1/5)
#' a_plot(diamonds, a_aes(carat)) +
#'   a_geom_density(adjust = 5)
#'
#' a_plot(diamonds, a_aes(depth, colour = cut)) +
#'   a_geom_density() +
#'   xlim(55, 70)
#' a_plot(diamonds, a_aes(depth, fill = cut, colour = cut)) +
#'   a_geom_density(alpha = 0.1) +
#'   xlim(55, 70)
#'
#' \donttest{
#' # Stacked density plots: if you want to create a stacked density plot, you
#' # probably want to 'count' (density * n) variable instead of the default
#' # density
#'
#' # Loses marginal densities
#' a_plot(diamonds, a_aes(carat, fill = cut)) +
#'   a_geom_density(a_position = "stack")
#' # Preserves marginal densities
#' a_plot(diamonds, a_aes(carat, ..count.., fill = cut)) +
#'   a_geom_density(a_position = "stack")
#'
#' # You can use a_position="fill" to produce a conditional density estimate
#' a_plot(diamonds, a_aes(carat, ..count.., fill = cut)) +
#'   a_geom_density(a_position = "fill")
#' }
a_geom_density <- function(mapping = NULL, data = NULL,
                         a_stat = "density", a_position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.a_aes = TRUE) {

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomDensity,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-ribbon.r
a_GeomDensity <- a_ggproto("a_GeomDensity", a_GeomArea,
  default_aes = defaults(
    a_aes(fill = NA, weight = 1, colour = "black", alpha = NA),
    a_GeomArea$default_aes
  )
)
