#' Add heatmap of 2d bin counts.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("stat", "bin2d")}
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_bin2d} and \code{stat_bin2d}.
#' @seealso \code{\link{stat_binhex}} for hexagonal binning
#' @examples
#' d <- ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10)
#' d + geom_bin2d()
#'
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + geom_bin2d(bins = 10)
#' d + geom_bin2d(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + geom_bin2d(binwidth = c(0.1, 0.1))
geom_bin2d <- function(mapping = NULL, data = NULL,
                       stat = "bin2d", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBin2d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-gganimintproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.r
GeomBin2d <- gganimintproto("GeomBin2d", GeomRect, 
extra_params = c("na.rm", "width", "height"),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  draw_key = draw_key_polygon,

  pre_process = function(g, g.data, ...) {
    stop("bin2d is not supported in animint. Try using geom_tile() and binning the data yourself.")
  }
)
