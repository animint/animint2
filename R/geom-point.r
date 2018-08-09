#' Points, as for a scatterplot
#'
#' The point geom is used to create scatterplots.
#'
#' The scatterplot is useful for displaying the relationship between two
#' continuous variables, although it can also be used with one continuous
#' and one categorical variable, or two categorical variables.  See
#' \code{\link{a_geom_jitter}} for possibilities.
#'
#' The \emph{bubblechart} is a scatterplot with a third variable mapped to
#' the size of points.  There are no special names for scatterplots where
#' another variable is mapped to point shape or colour, however.
#'
#' The biggest potential problem with a scatterplot is overplotting: whenever
#' you have more than a few points, points may be plotted on top of one
#' another. This can severely distort the visual appearance of the plot.
#' There is no one solution to this problem, but there are some techniques
#' that can help.  You can add additional information with
#' \code{\link{a_geom_smooth}}, \code{\link{a_geom_quantile}} or
#' \code{\link{a_geom_density_2d}}.  If you have few unique x values,
#' \code{\link{a_geom_boxplot}} may also be useful.  Alternatively, you can
#' summarise the number of points at each location and display that in some
#' way, using \code{\link{a_stat_sum}}. Another technique is to use transparent
#' points, e.g. \code{a_geom_point(alpha = 0.05)}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "point")}
#'
#' @seealso \code{\link{a_scale_size}} to see scale area of points, instead of
#'   radius, \code{\link{a_geom_jitter}} to jitter points to reduce (mild)
#'   overplotting
#' @inheritParams a_layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{a_layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired a_geom/a_stat.
#' @inheritParams a_layer
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(wt, mpg))
#' p + a_geom_point()
#'
#' # Add a_aesthetic mappings
#' p + a_geom_point(a_aes(colour = factor(cyl)))
#' p + a_geom_point(a_aes(shape = factor(cyl)))
#' p + a_geom_point(a_aes(size = qsec))
#'
#' # Change scales
#' p + a_geom_point(a_aes(colour = cyl)) + a_scale_colour_gradient(low = "blue")
#' p + a_geom_point(a_aes(shape = factor(cyl))) + a_scale_shape(solid = FALSE)
#'
#' # Set a_aesthetics to fixed value
#' a_plot(mtcars, a_aes(wt, mpg)) + a_geom_point(colour = "red", size = 3)
#'
#' \donttest{
#' # Varying alpha is useful for large datasets
#' d <- a_plot(diamonds, a_aes(carat, price))
#' d + a_geom_point(alpha = 1/10)
#' d + a_geom_point(alpha = 1/20)
#' d + a_geom_point(alpha = 1/100)
#' }
#'
#' # For shapes that have a border (like 21), you can colour the inside and
#' # outside separately. Use the stroke aesthetic to modify the width of the
#' # border
#' a_plot(mtcars, a_aes(wt, mpg)) +
#'   a_geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)
#'
#' \donttest{
#' # You can create interesting shapes by layering multiple points of
#' # different sizes
#' p <- a_plot(mtcars, a_aes(mpg, wt, shape = factor(cyl)))
#' p + a_geom_point(a_aes(colour = factor(cyl)), size = 4) +
#'   a_geom_point(colour = "grey90", size = 1.5)
#' p + a_geom_point(colour = "black", size = 4.5) +
#'   a_geom_point(colour = "pink", size = 4) +
#'   a_geom_point(a_aes(shape = factor(cyl)))
#'
#' # These extra layers don't usually appear in the legend, but we can
#' # force their inclusion
#' p + a_geom_point(colour = "black", size = 4.5, show.legend = TRUE) +
#'   a_geom_point(colour = "pink", size = 4, show.legend = TRUE) +
#'   a_geom_point(a_aes(shape = factor(cyl)))
#'
#' # a_geom_point warns when missing values have been dropped from the data set
#' # and not plotted, you can turn this off by setting na.rm = TRUE
#' mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
#' a_plot(mtcars2, a_aes(wt, mpg)) + a_geom_point()
#' a_plot(mtcars2, a_aes(wt, mpg)) + a_geom_point(na.rm = TRUE)
#' }
a_geom_point <- function(mapping = NULL, data = NULL,
                       a_stat = "identity", a_position = "identity",
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

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomPoint <- a_ggproto("a_GeomPoint", a_Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape"),
  default_aes = a_aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  draw_panel = function(data, panel_scales, a_coord, na.rm = FALSE) {
    coords <- a_coord$transform(data, panel_scales)
    ggname("geom_point",
      pointsGrob(
        coords$x, coords$y,
        pch = coords$shape,
        gp = gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          # Stroke is added around the outside of the point
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      )
    )
  },

  draw_key = a_draw_key_point
)
