#' @export
#' @rdname a_geom_linerange
a_geom_errorbar <- function(mapping = NULL, data = NULL,
                          a_stat = "identity", a_position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomErrorbar,
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
a_GeomErrorbar <- a_ggproto("a_GeomErrorbar", a_Geom,
  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, width = 0.5,
    alpha = NA),

  draw_key = a_draw_key_path,

  required_aes = c("x", "ymin", "ymax"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    transform(data,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw_panel = function(data, panel_scales, a_coord, width = NULL) {
    a_GeomPath$draw_panel(data.frame(
      x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x,    NA, data$xmin, data$xmax)),
      y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$ymin, NA, data$ymin, data$ymin)),
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
