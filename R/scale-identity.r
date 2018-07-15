#' Use values without scaling.
#'
#' @name a_scale_identity
#' @param ... Other arguments passed on to \code{\link{discrete_a_scale}} or
#'   \code{\link{continuous_a_scale}}
#' @param guide Guide to use for this scale - defaults to \code{"none"}.
#' @examples
#' a_plot(luv_colours, aes(u, v)) +
#'   geom_point(aes(colour = col), size = 3) +
#'   a_scale_color_identity() +
#'   coord_equal()
#'
#' df <- data.frame(
#'   x = 1:4,
#'   y = 1:4,
#'   colour = c("red", "green", "blue", "yellow")
#' )
#' a_plot(df, aes(x, y)) + geom_tile(aes(fill = colour))
#' a_plot(df, aes(x, y)) +
#'   geom_tile(aes(fill = colour)) +
#'   a_scale_fill_identity()
#'
#' # To get a legend guide, specify guide = "legend"
#' a_plot(df, aes(x, y)) +
#'   geom_tile(aes(fill = colour)) +
#'   a_scale_fill_identity(guide = "legend")
#' # But you'll typically also need to supply breaks and labels:
#' a_plot(df, aes(x, y)) +
#'   geom_tile(aes(fill = colour)) +
#'   a_scale_fill_identity("trt", labels = letters[1:4], breaks = df$colour,
#'   guide = "legend")
#'
#' # cyl scaled to appropriate size
#' a_plot(mtcars, aes(mpg, wt)) + geom_point(aes(size = cyl))
#'
#' # cyl used as point size
#' a_plot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(size = cyl)) +
#'   a_scale_size_identity()
NULL

#' @rdname a_scale_identity
#' @export
a_scale_colour_identity <- function(..., guide = "none") {
  sc <- discrete_a_scale("colour", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleDiscreteIdentity
  class(sc) <- class(a_ScaleDiscreteIdentity)
  sc
}

#' @rdname a_scale_identity
#' @export
a_scale_fill_identity <- function(..., guide = "none") {
  sc <- discrete_a_scale("fill", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleDiscreteIdentity
  class(sc) <- class(a_ScaleDiscreteIdentity)
  sc
}

#' @rdname a_scale_identity
#' @export
a_scale_shape_identity <- function(..., guide = "none") {
  sc <- continuous_a_scale("shape", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleContinuousIdentity
  class(sc) <- class(a_ScaleContinuousIdentity)
  sc
}

#' @rdname a_scale_identity
#' @export
a_scale_linetype_identity <- function(..., guide = "none") {
  sc <- discrete_a_scale("linetype", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleDiscreteIdentity
  class(sc) <- class(a_ScaleDiscreteIdentity)
  sc
}

#' @rdname a_scale_identity
#' @export
a_scale_alpha_identity <- function(..., guide = "none") {
  sc <- continuous_a_scale("alpha", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleContinuousIdentity
  class(sc) <- class(a_ScaleContinuousIdentity)
  sc
}

#' @rdname a_scale_identity
#' @export
a_scale_size_identity <- function(..., guide = "none") {
  sc <- continuous_a_scale("size", "identity", identity_pal(), ..., guide = guide)

  # TODO: Fix this hack. We're reassigning the parent a_ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- a_ScaleContinuousIdentity
  class(sc) <- class(a_ScaleContinuousIdentity)
  sc
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_ScaleDiscreteIdentity <- a_ggproto("a_ScaleDiscreteIdentity", a_ScaleDiscrete,
                                 map = function(x) {
                                   if (is.factor(x)) {
                                     as.character(x)
                                   } else {
                                     x
                                   }
                                 },

                                 train = function(self, x) {
                                   # do nothing if no guide, otherwise train so we know what breaks to use
                                   if (self$guide == "none") return()
                                   a_ggproto_parent(a_ScaleDiscrete, self)$train(x)
                                 }
)


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_ScaleContinuousIdentity <- a_ggproto("a_ScaleContinuousIdentity", a_ScaleContinuous,
                                   map = function(x) {
                                     if (is.factor(x)) {
                                       as.character(x)
                                     } else {
                                       x
                                     }
                                   },

                                   train = function(self, x) {
                                     # do nothing if no guide, otherwise train so we know what breaks to use
                                     if (self$guide == "none") return()
                                     a_ggproto_parent(a_ScaleDiscrete, self)$train(x)
                                   }
)
