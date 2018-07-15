#' Histograms and frequency polygons.
#'
#' Display a 1d distribution by dividing into bins and counting the number
#' of observations in each bin. Histograms use bars; frequency polygons use
#' lines.
#'
#' By default, \code{stat_bin} uses 30 bins - this is not a good default,
#' but the idea is to get you experimenting with different binwidths. You
#' may need to look at a few to uncover the full story behind your data.
#'
#' @section Aesthetics:
#' \code{geom_histogram} uses the same aesthetics as \code{geom_bar};
#' \code{geom_freqpoly} uses the same aesthetics as \code{geom_line}.
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_histogram}/\code{geom_freqpoly} and \code{stat_bin}.
#' @examples
#' a_plot(diamonds, aes(carat)) +
#'   geom_histogram()
#' a_plot(diamonds, aes(carat)) +
#'   geom_histogram(binwidth = 0.01)
#' a_plot(diamonds, aes(carat)) +
#'   geom_histogram(bins = 200)
#'
#' # Rather than stacking histograms, it's easier to compare frequency
#' # polygons
#' a_plot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(binwidth = 500)
#' a_plot(diamonds, aes(price, colour = cut)) +
#'   geom_freqpoly(binwidth = 500)
#'
#' # To make it easier to compare distributions with very different counts,
#' # put density on the y axis instead of the default count
#' a_plot(diamonds, aes(price, ..density.., colour = cut)) +
#'   geom_freqpoly(binwidth = 500)
#'
#' if (require("ggplot2movies")) {
#' # Often we don't want the height of the bar to represent the
#' # count of observations, but the sum of some other variable.
#' # For example, the following plot shows the number of movies
#' # in each rating.
#' m <- a_plot(movies, aes(rating))
#' m + geom_histogram(binwidth = 0.1)
#'
#' # If, however, we want to see the number of votes cast in each
#' # category, we need to weight by the votes variable
#' m + geom_histogram(aes(weight = votes), binwidth = 0.1) + ylab("votes")
#'
#' # For transformed scales, binwidth applies to the transformed data.
#' # The bins have constant width on the transformed scale.
#' m + geom_histogram() + a_scale_x_log10()
#' m + geom_histogram(binwidth = 0.05) + a_scale_x_log10()
#'
#' # For transformed coordinate systems, the binwidth applies to the
#' # raw data. The bins have constant width on the original scale.
#'
#' # Using log scales does not work here, because the first
#' # bar is anchored at zero, and so whens transformed becomes negative
#' # infinity. This is not a problem when transforming the scales, because
#' # no observations have 0 ratings.
#' m + geom_histogram(origin = 0) + coord_trans(x = "log10")
#' # Use origin = 0, to make sure we don't take sqrt of negative values
#' m + geom_histogram(origin = 0) + coord_trans(x = "sqrt")
#'
#' # You can also transform the y axis.  Remember that the base of the bars
#' # has value 0, so log transformations are not appropriate
#' m <- a_plot(movies, aes(x = rating))
#' m + geom_histogram(binwidth = 0.5) + a_scale_y_sqrt()
#' }
#' rm(movies)
geom_histogram <- function(mapping = NULL, data = NULL,
                           stat = "bin", position = "stack",
                           ...,
                           binwidth = NULL,
                           bins = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = a_GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      pad = FALSE,
      ...
    )
  )
}
