#' @export
#' @rdname a_geom_linerange
a_geom_pointrange <- function(mapping = NULL, data = NULL,
                            a_stat = "identity", a_position = "identity",
                            ...,
                            fatten = 4,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomPointrange,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomPointrange <- a_ggproto("a_GeomPointrange", a_Geom,
  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = a_draw_key_pointrange,

  required_aes = c("x", "y", "ymin", "ymax"),

  draw_panel = function(data, panel_scales, a_coord, fatten = 4) {
    if (is.null(data$y))
      return(a_GeomLinerange$draw_panel(data, panel_scales, a_coord))

    ggname("geom_pointrange",
      gTree(children = gList(
        a_GeomLinerange$draw_panel(data, panel_scales, a_coord),
        a_GeomPoint$draw_panel(transform(data, size = size * fatten), panel_scales, a_coord)
      ))
    )
  }
)
