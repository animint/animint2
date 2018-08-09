#' @export
#' @rdname a_geom_text
#' @param a_label.padding Amount of padding around a_label. Defaults to 0.25 lines.
#' @param a_label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param a_label.size Size of a_label border, in mm.
a_geom_label <- function(mapping = NULL, data = NULL,
                       a_stat = "identity", a_position = "identity",
                       ...,
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       a_label.padding = unit(0.25, "lines"),
                       a_label.r = unit(0.15, "lines"),
                       a_label.size = 0.25,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.a_aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(a_position)) {
      stop("Specify either `a_position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    a_position <- a_position_nudge(nudge_x, nudge_y)
  }

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomLabel,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      parse = parse,
      a_label.padding = a_label.padding,
      a_label.r = a_label.r,
      a_label.size = a_label.size,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomLabel <- a_ggproto("a_GeomLabel", a_Geom,
  required_aes = c("x", "y", "a_label"),

  default_aes = a_aes(
    colour = "black", fill = "white", size = 3.88, angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
    lineheight = 1.2
  ),

  draw_panel = function(self, data, panel_scales, a_coord, parse = FALSE,
                        na.rm = FALSE,
                        a_label.padding = unit(0.25, "lines"),
                        a_label.r = unit(0.15, "lines"),
                        a_label.size = 0.25) {
    lab <- data$a_label
    if (parse) {
      lab <- parse(text = as.character(lab))
    }

    data <- a_coord$transform(data, panel_scales)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    grobs <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      a_labelGrob(lab[i],
        x = unit(row$x, "native"),
        y = unit(row$y, "native"),
        just = c(row$hjust, row$vjust),
        padding = a_label.padding,
        r = a_label.r,
        text.gp = gpar(
          col = row$colour,
          fontsize = row$size * .pt,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        ),
        rect.gp = gpar(
          col = row$colour,
          fill = alpha(row$fill, row$alpha),
          lwd = a_label.size * .pt
        )
      )
    })
    class(grobs) <- "gList"

    ggname("geom_label", grobTree(children = grobs))
  },

  draw_key = a_draw_key_label
)

a_labelGrob <- function(a_label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {

  stopifnot(length(a_label) == 1)

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  gTree(a_label = a_label, x = x, y = y, just = just, padding = padding, r = r,
    name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "a_labelgrob")
}

#' @export
makeContent.a_labelgrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)

  t <- textGrob(
    x$a_label,
    x$x + 2 * (0.5 - hj) * x$padding,
    x$y + 2 * (0.5 - vj) * x$padding,
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )

  r <- roundrectGrob(x$x, x$y, default.units = "native",
    width = grobWidth(t) + 2 * x$padding,
    height = grobHeight(t) + 2 * x$padding,
    just = c(hj, vj),
    r = x$r,
    gp = x$rect.gp,
    name = "box"
  )

  setChildren(x, gList(r, t))
}
