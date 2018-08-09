#' Violin plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "violin")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param draw_quantiles If \code{not(NULL)} (default), draw horizontal lines
#'   at the given quantiles of the density estimate.
#' @param trim If \code{TRUE} (default), trim the tails of the violins
#'   to the range of the data. If \code{FALSE}, don't trim the tails.
#' @param a_geom,a_stat Use to override the default connection between
#'   \code{a_geom_violin} and \code{a_stat_ydensity}.
#' @export
#' @references Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box
#' Plot-Density Trace Synergism. The American Statistician 52, 181-184.
#' @examples
#' p <- a_plot(mtcars, a_aes(factor(cyl), mpg))
#' p + a_geom_violin()
#'
#' \donttest{
#' p + a_geom_violin() + a_geom_jitter(height = 0)
#' p + a_geom_violin() + a_coord_flip()
#'
#' # Scale maximum width proportional to sample size:
#' p + a_geom_violin(a_scale = "count")
#'
#' # Scale maximum width to 1 for all violins:
#' p + a_geom_violin(a_scale = "width")
#'
#' # Default is to trim violins to the range of the data. To disable:
#' p + a_geom_violin(trim = FALSE)
#'
#' # Use a smaller bandwidth for closer density fit (default is 1).
#' p + a_geom_violin(adjust = .5)
#'
#' # Add aesthetic mappings
#' # Note that violins are automatically dodged when any aesthetic is
#' # a factor
#' p + a_geom_violin(a_aes(fill = cyl))
#' p + a_geom_violin(a_aes(fill = factor(cyl)))
#' p + a_geom_violin(a_aes(fill = factor(vs)))
#' p + a_geom_violin(a_aes(fill = factor(am)))
#'
#' # Set aesthetics to fixed value
#' p + a_geom_violin(fill = "grey80", colour = "#3366FF")
#'
#' # Show quartiles
#' p + a_geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
#'
#' # Scales vs. coordinate transforms -------
#' if (require("ggplot2movies")) {
#' # Scale transformations occur before the density statistics are computed.
#' # Coordinate transformations occur afterwards.  Observe the effect on the
#' # number of outliers.
#' m <- a_plot(movies, a_aes(y = votes, x = rating, group = cut_width(rating, 0.5)))
#' m + a_geom_violin()
#' m + a_geom_violin() + a_scale_y_log10()
#' m + a_geom_violin() + a_coord_trans(y = "log10")
#' m + a_geom_violin() + a_scale_y_log10() + a_coord_trans(y = "log10")
#'
#' # Violin plots with continuous x:
#' # Use the group aesthetic to group observations in violins
#' a_plot(movies, a_aes(year, budget)) + a_geom_violin()
#' a_plot(movies, a_aes(year, budget)) +
#'   a_geom_violin(a_aes(group = cut_width(year, 10)), a_scale = "width")
#' }
#' }
a_geom_violin <- function(mapping = NULL, data = NULL,
                        a_stat = "ydensity", a_position = "dodge",
                        ...,
                        draw_quantiles = NULL,
                        trim = TRUE,
                        a_scale = "area",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomViolin,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      trim = trim,
      a_scale = a_scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomViolin <- a_ggproto("a_GeomViolin", a_Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    plyr::ddply(data, "group", transform,
      xmin = x - width / 2,
      xmax = x + width / 2
    )
  },

  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    # Find the points for the line to go all the way around
    data <- transform(data,
      xminv = x - violinwidth * (x - xmin),
      xmaxv = x + violinwidth * (xmax - x)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      plyr::arrange(transform(data, x = xminv), y),
      plyr::arrange(transform(data, x = xmaxv), -y)
    )

    # Close the polygon: set first and last point the same
    # Needed for a_coord_polar and such
    newdata <- rbind(newdata, newdata[1,])

    # Draw quantiles if requested
    if (length(draw_quantiles) > 0) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))

      # Compute the quantile segments and combine with existing aesthetics
      quantiles <- create_quantile_segment_frame(data, draw_quantiles)
      a_aesthetics <- data[
        rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y")),
        drop = FALSE
      ]
      both <- cbind(quantiles, a_aesthetics)
      quantile_grob <- a_GeomPath$draw_panel(both, ...)

      ggname("geom_violin", grobTree(
        a_GeomPolygon$draw_panel(newdata, ...),
        quantile_grob)
      )
    } else {
      ggname("geom_violin", a_GeomPolygon$draw_panel(newdata, ...))
    }
  },

  draw_key = a_draw_key_polygon,

  default_aes = a_aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, linetype = "solid"),

  required_aes = c("x", "y")
)

# Returns a data.frame with info needed to draw quantile segments.
create_quantile_segment_frame <- function(data, draw_quantiles) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles) # these are all the y-values for quantiles

  # Get the violin bounds for the requested quantiles
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)

  # We have two rows per segment drawn. Each segments gets its own group.
  data.frame(
    x = interleave(violin.xminvs, violin.xmaxvs),
    y = rep(ys, each = 2),
    group = rep(ys, each = 2)
  )
}

