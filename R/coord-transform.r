#' Transformed cartesian coordinate system.
#'
#' \code{a_coord_trans} is different to scale transformations in that it occurs after
#' statistical transformation and will affect the visual appearance of geoms - there is
#' no guarantee that straight lines will continue to be straight.
#'
#' All current transformations only work with continuous values - see
#' \code{\link[scales]{trans_new}} for list of transformations, and instructions on
#' how to create your own.
#'
#' @param x,y transformers for x and y axes
#' @param xtrans,ytrans Deprecated; use \code{x} and \code{y} instead.
#' @param limx,limy limits for x and y axes. (Named so for backward
#'    compatibility)
#' @export
#' @examples
#' \donttest{
#' # See ?a_geom_boxplot for other examples
#'
#' # Three ways of doing transformation in ggplot:
#' #  * by transforming the data
#' a_plot(diamonds, a_aes(log10(carat), log10(price))) +
#'   a_geom_point()
#' #  * by transforming the scales
#' a_plot(diamonds, a_aes(carat, price)) +
#'   a_geom_point() +
#'   a_scale_x_log10() +
#'   a_scale_y_log10()
#' #  * by transforming the coordinate system:
#' a_plot(diamonds, a_aes(carat, price)) +
#'   a_geom_point() +
#'   a_coord_trans(x = "log10", y = "log10")
#'
#' # The difference between transforming the scales and
#' # transforming the coordinate system is that scale
#' # transformation occurs BEFORE statistics, and coordinate
#' # transformation afterwards.  Coordinate transformation also
#' # changes the shape of geoms:
#'
#' d <- subset(diamonds, carat > 0.5)
#'
#' a_plot(d, a_aes(carat, price)) +
#'   a_geom_point() +
#'   a_geom_smooth(method = "lm") +
#'   a_scale_x_log10() +
#'   a_scale_y_log10()
#'
#' a_plot(d, a_aes(carat, price)) +
#'   a_geom_point() +
#'   a_geom_smooth(method = "lm") +
#'   a_coord_trans(x = "log10", y = "log10")
#'
#' # Here I used a subset of diamonds so that the smoothed line didn't
#' # drop below zero, which obviously causes problems on the log-transformed
#' # scale
#'
#' # With a combination of scale and coordinate transformation, it's
#' # possible to do back-transformations:
#' a_plot(diamonds, a_aes(carat, price)) +
#'   a_geom_point() +
#'   a_geom_smooth(method = "lm") +
#'   a_scale_x_log10() +
#'   a_scale_y_log10() +
#'   a_coord_trans(x = scales::exp_trans(10), y = scales::exp_trans(10))
#'
#' # cf.
#' a_plot(diamonds, a_aes(carat, price)) +
#'   a_geom_point() +
#'   a_geom_smooth(method = "lm")
#'
#' # Also works with discrete scales
#' df <- data.frame(a = abs(rnorm(26)),letters)
#' plot <- a_plot(df,a_aes(a,letters)) + a_geom_point()
#'
#' plot + a_coord_trans(x = "log10")
#' plot + a_coord_trans(x = "sqrt")
#' }
a_coord_trans <- function(x = "identity", y = "identity", limx = NULL, limy = NULL,
  xtrans, ytrans)
{
  if (!missing(xtrans)) {
    gg_dep("1.0.1", "`xtrans` arguments is deprecated; please use `x` instead.")
    x <- xtrans
  }
  if (!missing(ytrans)) {
    gg_dep("1.0.1", "`ytrans` arguments is deprecated; please use `y` instead.")
    y <- ytrans
  }

  # @kohske
  # Now limits are implemented.
  # But for backward compatibility, xlim -> limx, ylim -> ylim
  # Because there are many examples such as
  # > a_coord_trans(x = "log10", y = "log10")
  # Maybe this is changed.
  if (is.character(x)) x <- as.trans(x)
  if (is.character(y)) y <- as.trans(y)


  a_ggproto(NULL, a_CoordTrans,
    trans = list(x = x, y = y),
    limits = list(x = limx, y = limy)
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_CoordTrans <- a_ggproto("a_CoordTrans", a_Coord,

  distance = function(self, x, y, scale_details) {
    max_dist <- dist_euclidean(scale_details$x.range, scale_details$y.range)
    dist_euclidean(self$trans$x$transform(x), self$trans$y$transform(y)) / max_dist
  },

  transform = function(self, data, scale_details) {
    trans_x <- function(data) transform_value(self$trans$x, data, scale_details$x.range)
    trans_y <- function(data) transform_value(self$trans$y, data, scale_details$y.range)

    data <- transform_position(data, trans_x, trans_y)
    transform_position(data, squish_infinite, squish_infinite)
  },

  train = function(self, scale_details) {
    c(train_trans(scale_details$x, self$limits$x, self$trans$x, "x"),
      train_trans(scale_details$y, self$limits$y, self$trans$y, "y"))
  }
)

transform_value <- function(trans, value, range) {
  if (is.null(value))
    return(value)
  rescale(trans$transform(value), 0:1, range)
}


train_trans <- function(scale_details, limits, trans, name) {
  # first, calculate the range that is the numerical limits in data space

  # expand defined by scale OR coord
  # @kohske
  # Expansion of data range sometimes go beyond domain,
  # so in trans, expansion takes place at the final stage.
  if (is.null(limits)) {
    range <- scale_details$dimension()
  } else {
    range <- range(scale_details$transform(limits))
  }

  # breaks on data space
  out <- scale_details$break_info(range)

  # trans'd range
  out$range <- trans$transform(out$range)

  # expansion if limits are not specified
  if (is.null(limits)) {
    expand <- expand_default(scale_details)
    out$range <- expand_range(out$range, expand[1], expand[2])
  }

  # major and minor values in plot space
  out$major_source <- transform_value(trans, out$major_source, out$range)
  out$minor_source <- transform_value(trans, out$minor_source, out$range)

  out <- list(
    range = out$range, a_labels = out$a_labels,
    major = out$major_source, minor = out$minor_source
  )
  names(out) <- paste(name, names(out), sep = ".")
  out
}
