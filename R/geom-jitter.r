#' Points, jittered to reduce overplotting.
#'
#' The jitter geom is a convenient default for a_geom_point with a_position =
#' 'jitter'. It's a useful way of handling overplotting caused by discreteness
#' in smaller datasets.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "point")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @inheritParams a_position_jitter
#' @seealso
#'  \code{\link{a_geom_point}} for regular, unjittered points,
#'  \code{\link{a_geom_boxplot}} for another way of looking at the conditional
#'     distribution of a variable
#' @export
#' @examples
#' p <- a_plot(mpg, a_aes(cyl, hwy))
#' p + a_geom_point()
#' p + a_geom_jitter()
#'
#' # Add aesthetic mappings
#' p + a_geom_jitter(a_aes(colour = class))
#'
#' # Use smaller width/height to emphasise categories
#' a_plot(mpg, a_aes(cyl, hwy)) + a_geom_jitter()
#' a_plot(mpg, a_aes(cyl, hwy)) + a_geom_jitter(width = 0.25)
#'
#' # Use larger width/height to completely smooth away discreteness
#' a_plot(mpg, a_aes(cty, hwy)) + a_geom_jitter()
#' a_plot(mpg, a_aes(cty, hwy)) + a_geom_jitter(width = 0.5, height = 0.5)
a_geom_jitter <- function(mapping = NULL, data = NULL,
                        a_stat = "identity", a_position = "jitter",
                        ...,
                        width = NULL,
                        height = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.a_aes = TRUE) {
  if (!missing(width) || !missing(height)) {
    if (!missing(a_position)) {
      stop("Specify either `a_position` or `width`/`height`", call. = FALSE)
    }

    a_position <- a_position_jitter(width = width, height = height)
  }

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
