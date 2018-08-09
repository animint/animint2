#' @include stat-.r
NULL

#' Lines: horizontal, vertical, and specified by slope and intercept.
#'
#' These paired geoms and stats add straight lines to a plot, either
#' horizontal, vertical or specified by slope and intercept. These are useful
#' for annotating plots.
#'
#' These geoms act slightly different to other geoms. You can supply the
#' parameters in two ways: either as arguments to the a_layer function,
#' or via aesthetics. If you use arguments, e.g.
#' \code{a_geom_abline(intercept = 0, slope = 1)}, then behind the scenes
#' the geom makes a new data frame containing just the data you've supplied.
#' That means that the lines will be the same in all facets; if you want them
#' to vary across facets, construct the data frame yourself and use aesthetics.
#'
#' Unlike most other geoms, these geoms do not inherit aesthetics from the plot
#' default, because they do not understand x and y aesthetics which are
#' commonly set in the plot. They also do not affect the x and y scales.
#'
#' @section Aesthetics:
#' These geoms are drawn using with \code{\link{a_geom_line}} so support the
#' same aesthetics: alpha, colour, linetype and size. They also each have
#' aesthetics that control the position of the line:
#'
#' \itemize{
#'   \item \code{a_geom_vline}: \code{xintercept}
#'   \item \code{a_geom_hline}: \code{yintercept}
#'   \item \code{a_geom_abline}: \code{slope} and \code{intercept}
#' }
#'
#' @seealso See \code{\link{a_geom_segment}} for a more general approach to
#'   adding straight line segments to a plot.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line. If these are set, \code{data}, \code{mapping} and
#'   \code{show.legend} are overridden
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(wt, mpg)) + a_geom_point()
#'
#' # Fixed values
#' p + a_geom_vline(xintercept = 5)
#' p + a_geom_vline(xintercept = 1:5)
#' p + a_geom_hline(yintercept = 20)
#'
#' p + a_geom_abline() # Can't see it - outside the range of the data
#' p + a_geom_abline(intercept = 20)
#'
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + a_geom_abline(intercept = 37, slope = -5)
#' # But this is easier to do with a_geom_smooth:
#' p + a_geom_smooth(method = "lm", se = FALSE)
#'
#' # To show different lines in different facets, use aesthetics
#' p <- a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point() +
#'   a_facet_wrap(~ cyl)
#'
#' mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
#' p + a_geom_hline(a_aes(yintercept = wt), mean_wt)
#'
#' # You can also control other aesthetics
#' a_plot(mtcars, a_aes(mpg, wt, colour = wt)) +
#'   a_geom_point() +
#'   a_geom_hline(a_aes(yintercept = wt, colour = wt), mean_wt) +
#'   a_facet_wrap(~ cyl)
a_geom_abline <- function(mapping = NULL, data = NULL,
                        ...,
                        slope,
                        intercept,
                        na.rm = FALSE,
                        show.legend = NA) {

  # If nothing set, default to y = x
  if (missing(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {
    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0

    data <- data.frame(intercept = intercept, slope = slope)
    mapping <- a_aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatIdentity,
    a_geom = a_GeomAbline,
    a_position = a_PositionIdentity,
    show.legend = show.legend,
    inherit.a_aes = FALSE,
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
a_GeomAbline <- a_ggproto("a_GeomAbline", a_Geom,
  draw_panel = function(data, panel_scales, a_coord) {
    ranges <- a_coord$range(panel_scales)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept

    a_GeomSegment$draw_panel(unique(data), panel_scales, a_coord)
  },

  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  required_aes = c("slope", "intercept"),

  draw_key = a_draw_key_abline
)
