#' Scale for shapes, aka glyphs.
#'
#' A continuous variable can not be mapped to shape.
#'
#' @param solid Are the shapes solid, \code{TRUE}, or hollow \code{FALSE}?
#' @inheritParams a_scale_x_discrete
#' @rdname a_scale_shape
#' @export
#' @examples
#' dsmall <- diamonds[sample(nrow(diamonds), 100), ]
#'
#' (d <- a_plot(dsmall, a_aes(carat, price)) + a_geom_point(a_aes(shape = cut)))
#' d + a_scale_shape(solid = TRUE) # the default
#' d + a_scale_shape(solid = FALSE)
#' d + a_scale_shape(name = "Cut of diamond")
#' d + a_scale_shape(name = "Cut of\ndiamond")
#'
#' # To change order of levels, change order of
#' # underlying factor
#' levels(dsmall$cut) <- c("Fair", "Good", "Very Good", "Premium", "Ideal")
#'
#' # Need to recreate plot to pick up new data
#' a_plot(dsmall, a_aes(price, carat)) + a_geom_point(a_aes(shape = cut))
#'
#' # Or for short:
#' d %+% dsmall
a_scale_shape <- function(..., solid = TRUE) {
  discrete_a_scale("shape", "shape_d", shape_pal(solid), ...)
}

#' @rdname a_scale_shape
#' @export
#' @usage NULL
a_scale_shape_discrete <- a_scale_shape

#' @rdname a_scale_shape
#' @export
#' @usage NULL
a_scale_shape_continuous <- function(...) {
  stop("A continuous variable can not be mapped to shape", call. = FALSE)
}
