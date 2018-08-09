#' Histograms and frequency polygons.
#'
#' Display a 1d distribution by dividing into bins and counting the number
#' of observations in each bin. Histograms use bars; frequency polygons use
#' lines.
#'
#' By default, \code{a_stat_bin} uses 30 bins - this is not a good default,
#' but the idea is to get you experimenting with different binwidths. You
#' may need to look at a few to uncover the full story behind your data.
#'
#' @section Aesthetics:
#' \code{a_geom_histogram} uses the same aesthetics as \code{a_geom_bar};
#' \code{a_geom_freqpoly} uses the same aesthetics as \code{a_geom_line}.
#'
#' @export
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param a_geom,a_stat Use to override the default connection between
#'   \code{a_geom_histogram}/\code{a_geom_freqpoly} and \code{a_stat_bin}.
#' @examples
#' a_plot(diamonds, a_aes(carat)) +
#'   a_geom_histogram()
#' a_plot(diamonds, a_aes(carat)) +
#'   a_geom_histogram(binwidth = 0.01)
#' a_plot(diamonds, a_aes(carat)) +
#'   a_geom_histogram(bins = 200)
#'
#' # Rather than stacking histograms, it's easier to compare frequency
#' # polygons
#' a_plot(diamonds, a_aes(price, fill = cut)) +
#'   a_geom_histogram(binwidth = 500)
#' a_plot(diamonds, a_aes(price, colour = cut)) +
#'   a_geom_freqpoly(binwidth = 500)
#'
#' # To make it easier to compare distributions with very different counts,
#' # put density on the y axis instead of the default count
#' a_plot(diamonds, a_aes(price, ..density.., colour = cut)) +
#'   a_geom_freqpoly(binwidth = 500)
#'
#' if (require("ggplot2movies")) {
#' # Often we don't want the height of the bar to represent the
#' # count of observations, but the sum of some other variable.
#' # For example, the following plot shows the number of movies
#' # in each rating.
#' m <- a_plot(movies, a_aes(rating))
#' m + a_geom_histogram(binwidth = 0.1)
#'
#' # If, however, we want to see the number of votes cast in each
#' # category, we need to weight by the votes variable
#' m + a_geom_histogram(a_aes(weight = votes), binwidth = 0.1) + ylab("votes")
#'
#' # For transformed scales, binwidth applies to the transformed data.
#' # The bins have constant width on the transformed scale.
#' m + a_geom_histogram() + a_scale_x_log10()
#' m + a_geom_histogram(binwidth = 0.05) + a_scale_x_log10()
#'
#' # For transformed coordinate systems, the binwidth applies to the
#' # raw data. The bins have constant width on the original scale.
#'
#' # Using log scales does not work here, because the first
#' # bar is anchored at zero, and so whens transformed becomes negative
#' # infinity. This is not a problem when transforming the scales, because
#' # no observations have 0 ratings.
#' m + a_geom_histogram(origin = 0) + a_coord_trans(x = "log10")
#' # Use origin = 0, to make sure we don't take sqrt of negative values
#' m + a_geom_histogram(origin = 0) + a_coord_trans(x = "sqrt")
#'
#' # You can also transform the y axis.  Remember that the base of the bars
#' # has value 0, so log transformations are not appropriate
#' m <- a_plot(movies, a_aes(x = rating))
#' m + a_geom_histogram(binwidth = 0.5) + a_scale_y_sqrt()
#' }
#' rm(movies)
a_geom_histogram <- function(mapping = NULL, data = NULL,
                           a_stat = "bin", a_position = "stack",
                           ...,
                           binwidth = NULL,
                           bins = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.a_aes = TRUE) {

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomBar,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      pad = FALSE,
      ...
    )
  )
}
