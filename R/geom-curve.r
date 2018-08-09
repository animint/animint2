#' @inheritParams grid::curveGrob
#' @export
#' @rdname a_geom_segment
a_geom_curve <- function(mapping = NULL, data = NULL,
                       a_stat = "identity", a_position = "identity",
                       ...,
                       curvature = 0.5,
                       angle = 90,
                       ncp = 5,
                       arrow = NULL,
                       lineend = "butt",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomCurve,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @include geom-segment.r
#' @format NULL
#' @usage NULL
#' @export
a_GeomCurve <- a_ggproto("a_GeomCurve", a_GeomSegment,
  draw_panel = function(data, panel_scales, a_coord, curvature = 0.5, angle = 90,
                        ncp = 5, arrow = NULL, lineend = "butt", na.rm = FALSE) {
    if (!a_coord$is_linear()) {
      warning("a_geom_curve is not implemented for non-linear coordinates",
        call. = FALSE)
    }
    trans <- a_coord$transform(data, panel_scales)
    curveGrob(
      trans$x, trans$y, trans$xend, trans$yend,
      default.units = "native",
      curvature = curvature, angle = angle, ncp = ncp,
      square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
      gp = gpar(
        col = alpha(trans$colour, trans$alpha),
        lwd = trans$size * .pt,
        lty = trans$linetype,
        lineend = trans$lineend),
      arrow = arrow
    )
  }
)
