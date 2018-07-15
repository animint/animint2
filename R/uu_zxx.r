# Default scales -------------------------------------------------------------

#' @export
#' @rdname a_scale_hue
#' @usage NULL
a_scale_colour_discrete <- a_scale_colour_hue

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_colour_continuous <- a_scale_colour_gradient

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_colour_datetime <- function() {
  a_scale_colour_continuous(trans = "time")
}

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_colour_date <- function() {
  a_scale_colour_continuous(trans = "date")
}

#' @export
#' @rdname a_scale_hue
#' @usage NULL
a_scale_fill_discrete <- a_scale_fill_hue

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_fill_continuous <- a_scale_fill_gradient

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_fill_datetime <- function() {
  a_scale_fill_continuous(trans = "time")
}

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_fill_date <- function() {
  a_scale_fill_continuous(trans = "date")
}


# British to American spellings ----------------------------------------------

#' @export
#' @rdname a_scale_brewer
#' @usage NULL
a_scale_color_brewer <- a_scale_colour_brewer

#' @export
#' @rdname a_scale_brewer
#' @usage NULL
a_scale_color_distiller <- a_scale_colour_distiller

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_color_continuous <- a_scale_colour_gradient

#' @export
#' @rdname a_scale_hue
#' @usage NULL
a_scale_color_discrete <- a_scale_colour_hue

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_color_gradient <- a_scale_colour_gradient

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_color_gradient2 <- a_scale_colour_gradient2

#' @export
#' @rdname a_scale_gradient
#' @usage NULL
a_scale_color_gradientn <- a_scale_colour_gradientn

#' @export
#' @rdname a_scale_grey
#' @usage NULL
a_scale_color_grey <- a_scale_colour_grey

#' @export
#' @rdname a_scale_hue
#' @usage NULL
a_scale_color_hue <- a_scale_colour_hue

#' @export
#' @rdname a_scale_identity
#' @usage NULL
a_scale_color_identity <- a_scale_colour_identity

#' @export
#' @rdname a_scale_manual
#' @usage NULL
a_scale_color_manual <- a_scale_colour_manual
