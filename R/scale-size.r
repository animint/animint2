#' Scale size (area or radius).
#'
#' \code{a_scale_size} scales area, \code{a_scale_radius} scales radius. The size
#' aesthetic is most commonly used for points and text, and humans perceive
#' the area of points (not their radius), so this provides for optimal
#' perception. \code{a_scale_size_area} ensures that a value of 0 is mapped
#' to a size of 0.
#'
#' @name a_scale_size
#' @inheritParams continuous_a_scale
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @seealso \code{\link{a_scale_size_area}} if you want 0 values to be mapped
#'   to points with size 0.
#' @examples
#' p <- a_plot(mpg, a_aes(displ, hwy, size = hwy)) +
#'    a_geom_point()
#' p
#' p + a_scale_size("Highway mpg")
#' p + a_scale_size(range = c(0, 10))
#'
#' # If you want zero value to have zero size, use a_scale_size_area:
#' p + a_scale_size_area()
#'
#' # This is most useful when size is a count
#' a_plot(mpg, a_aes(class, cyl)) +
#'   a_geom_count() +
#'   a_scale_size_area()
#'
#' # If you want to map size to radius (usually bad idea), use a_scale_radius
#' p + a_scale_radius()
NULL

#' @rdname a_scale_size
#' @export
#' @usage NULL
a_scale_size_continuous <- function(name = waiver(), breaks = waiver(), a_labels = waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", a_guide = "legend") {
  continuous_a_scale("size", "area", area_pal(range), name = name,
    breaks = breaks, a_labels = a_labels, limits = limits, trans = trans,
    a_guide = a_guide)
}

#' @rdname a_scale_size
#' @export
a_scale_radius <- function(name = waiver(), breaks = waiver(), a_labels = waiver(),
                         limits = NULL, range = c(1, 6),
                         trans = "identity", a_guide = "legend") {
  continuous_a_scale("size", "radius", rescale_pal(range), name = name,
    breaks = breaks, a_labels = a_labels, limits = limits, trans = trans,
    a_guide = a_guide)
}

#' @rdname a_scale_size
#' @export
a_scale_size <- a_scale_size_continuous

#' @rdname a_scale_size
#' @export
#' @usage NULL
a_scale_size_discrete <- function(..., range = c(2, 6)) {
  warning("Using size for a discrete variable is not advised.", call. = FALSE)
  discrete_a_scale("size", "size_d", function(n) {
    area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
    sqrt(area)
  }, ...)
}

#' @param ... Other arguments passed on to \code{\link{continuous_a_scale}}
#'   to control name, limits, breaks, a_labels and so forth.
#' @param max_size Size of largest points.
#' @export
#' @rdname a_scale_size
a_scale_size_area <- function(..., max_size = 6) {
  continuous_a_scale("size", "area",
    palette = abs_area(max_size),
    rescaler = rescale_max, ...)
}

#' @rdname a_scale_size
#' @export
#' @usage NULL
a_scale_size_datetime <- function() {
  a_scale_size_continuous(trans = "time")
}

#' @rdname a_scale_size
#' @export
#' @usage NULL
a_scale_size_date <- function() {
  a_scale_size_continuous(trans = "date")
}

