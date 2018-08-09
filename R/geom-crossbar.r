#' @export
#' @rdname a_geom_linerange
a_geom_crossbar <- function(mapping = NULL, data = NULL,
                          a_stat = "identity", a_position = "identity",
                          ...,
                          fatten = 2.5,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomCrossbar,
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
a_GeomCrossbar <- a_ggproto("a_GeomCrossbar", a_Geom,
  setup_data = function(data, params) {
    a_GeomErrorbar$setup_data(data, params)
  },

  default_aes = a_aes(colour = "black", fill = NA, size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y", "ymin", "ymax"),

  draw_key = a_draw_key_crossbar,

  draw_panel = function(data, panel_scales, a_coord, fatten = 2.5, width = NULL) {
    middle <- transform(data, x = xmin, xend = xmax, yend = y, size = size * fatten, alpha = NA)

    has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
      !is.na(data$ynotchlower) && !is.na(data$ynotchupper)

    if (has_notch) {
      if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
        message("notch went outside hinges. Try setting notch=FALSE.")

      notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

      middle$x <- middle$x + notchindent
      middle$xend <- middle$xend - notchindent

      box <- data.frame(
        x = c(
          data$xmin, data$xmin, data$xmin + notchindent, data$xmin, data$xmin,
          data$xmax, data$xmax, data$xmax - notchindent, data$xmax, data$xmax,
          data$xmin
        ),
        y = c(
          data$ymax, data$ynotchupper, data$y, data$ynotchlower, data$ymin,
          data$ymin, data$ynotchlower, data$y, data$ynotchupper, data$ymax,
          data$ymax
        ),
        alpha = data$alpha,
        colour = data$colour,
        size = data$size,
        linetype = data$linetype, fill = data$fill,
        group = seq_len(nrow(data)),
        stringsAsFactors = FALSE
      )
    } else {
      # No notch
      box <- data.frame(
        x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
        y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
        alpha = data$alpha,
        colour = data$colour,
        size = data$size,
        linetype = data$linetype,
        fill = data$fill,
        group = seq_len(nrow(data)), # each bar forms it's own group
        stringsAsFactors = FALSE
      )
    }

    ggname("geom_crossbar", gTree(children = gList(
      a_GeomPolygon$draw_panel(box, panel_scales, a_coord),
      a_GeomSegment$draw_panel(middle, panel_scales, a_coord)
    )))
  }
)
