#' @include stat-.r
NULL

#' @export
#' @rdname a_geom_abline
a_geom_vline <- function(mapping = NULL, data = NULL,
                       ...,
                       xintercept,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(xintercept)) {
    data <- data.frame(xintercept = xintercept)
    mapping <- a_aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatIdentity,
    a_geom = a_GeomVline,
    a_position = a_PositionIdentity,
    show.legend = show.legend,
    inherit.a_aes = FALSE,
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
a_GeomVline <- a_ggproto("a_GeomVline", a_Geom,
  draw_panel = function(data, panel_scales, a_coord) {
    ranges <- a_coord$range(panel_scales)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    a_GeomSegment$draw_panel(unique(data), panel_scales, a_coord)
  },

  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  required_aes = "xintercept",

  draw_key = a_draw_key_vline
)
