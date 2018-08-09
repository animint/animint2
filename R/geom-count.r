#' Count the number of observations at each location.
#'
#' This is a variant \code{\link{a_geom_point}} that counts the number of
#' observations at each location, then maps the count to point size. It
#' useful when you have discrete data.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "point")}
#' @param a_geom,a_stat Use to override the default connection between
#'   \code{a_geom_count} and \code{a_stat_sum}.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' a_plot(mpg, a_aes(cty, hwy)) +
#'  a_geom_point()
#'
#' a_plot(mpg, a_aes(cty, hwy)) +
#'  a_geom_count()
#'
#' # Best used in conjunction with a_scale_size_area which ensures that
#' # counts of zero would be given size 0. Doesn't make much different
#' # here because the smallest count is already close to 0.
#' a_plot(mpg, a_aes(cty, hwy)) +
#'  a_geom_count()
#'  a_scale_size_area()
#'
#' # Display proportions instead of counts -------------------------------------
#' # By default, all categorical variables in the plot form the groups.
#' # Specifying a_geom_count without a group identifier leads to a plot which is
#' # not useful:
#' d <- a_plot(diamonds, a_aes(x = cut, y = clarity))
#' d + a_geom_count(a_aes(size = ..prop..))
#' # To correct this problem and achieve a more desirable plot, we need
#' # to specify which group the proportion is to be calculated over.
#' d + a_geom_count(a_aes(size = ..prop.., group = 1)) +
#'   a_scale_size_area(max_size = 10)
#'
#' # Or group by x/y variables to have rows/columns sum to 1.
#' d + a_geom_count(a_aes(size = ..prop.., group = cut)) +
#'   a_scale_size_area(max_size = 10)
#' d + a_geom_count(a_aes(size = ..prop.., group = clarity)) +
#'   a_scale_size_area(max_size = 10)
a_geom_count <- function(mapping = NULL, data = NULL,
                       a_stat = "sum", a_position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomPoint,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
