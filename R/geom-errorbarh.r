#' Horizontal error bars
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "errorbarh")}
#'
#' @seealso \code{\link{a_geom_errorbar}}: vertical error bars
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   se = c(0.1, 0.3, 0.3, 0.2)
#' )
#'
#' # Define the top and bottom of the errorbars
#'
#' p <- a_plot(df, a_aes(resp, trt, colour = group))
#' p + a_geom_point() +
#'   a_geom_errorbarh(a_aes(xmax = resp + se, xmin = resp - se))
#' p + a_geom_point() +
#'   a_geom_errorbarh(a_aes(xmax = resp + se, xmin = resp - se, height = .2))
a_geom_errorbarh <- function(mapping = NULL, data = NULL,
                           a_stat = "identity", a_position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomErrorbarh,
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
a_GeomErrorbarh <- a_ggproto("a_GeomErrorbarh", a_Geom,
  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, height = 0.5,
    alpha = NA),

  draw_key = a_draw_key_path,

  required_aes = c("x", "xmin", "xmax", "y"),

  setup_data = function(data, params) {
    data$height <- data$height %||%
      params$height %||% (resolution(data$y, FALSE) * 0.9)

    transform(data,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  draw_panel = function(data, panel_scales, a_coord, height = NULL) {
    a_GeomPath$draw_panel(data.frame(
      x = as.vector(rbind(data$xmax, data$xmax, NA, data$xmax, data$xmin, NA, data$xmin, data$xmin)),
      y = as.vector(rbind(data$ymin, data$ymax, NA, data$y,    data$y,    NA, data$ymin, data$ymax)),
      colour = rep(data$colour, each = 8),
      alpha = rep(data$alpha, each = 8),
      size = rep(data$size, each = 8),
      linetype = rep(data$linetype, each = 8),
      group = rep(1:(nrow(data)), each = 8),
      stringsAsFactors = FALSE,
      row.names = 1:(nrow(data) * 8)
    ), panel_scales, a_coord)
  }
)
