#' Aligned label boxes
#'
#' This geom creates boxes with labels that are aligned either vertically or horizontally,
#' using quadratic programming to optimize their positions and avoid overlaps.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#' @param alignment One of "vertical" or "horizontal" to specify the alignment direction.
#' @param min_distance Minimum distance between boxes in native units.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   x = c(1, 2, 3),
#'   y = c(1, 2, 3),
#'   label = c("A", "B", "C")
#' )
#' geom_aligned_boxes(df, aes(x, y, label = label))
geom_aligned_boxes <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              label.padding = unit(0.25, "lines"),
                              label.r = unit(0.15, "lines"),
                              label.size = 0.25,
                              alignment = "vertical",
                              min_distance = 0.1,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAlignedBoxes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      alignment = alignment,
      min_distance = min_distance,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_aligned_boxes
#' @export
GeomAlignedBoxes <- gganimintproto("GeomAlignedBoxes", Geom,
  required_aes = c("x", "y", "label"),
  
  default_aes = aes(
    colour = "#a70000", fill = "#fa8181", size = 3.88, 
    angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA,
    family = "", fontface = 1, lineheight = 1.2, fontsize = 12
  ),

  draw_panel = function(self, data, panel_scales, coord,
                       label.padding = unit(0.25, "lines"),
                       label.r = unit(0.15, "lines"),
                       label.size = 0.25,
                       alignment = "vertical",
                       min_distance = 0.1,
                       na.rm = FALSE) {
    
    if (empty(data)) return(zeroGrob())

    coords <- coord$transform(data, panel_scales)

    coords$label.padding <- convertWidth(label.padding, "native", valueOnly = TRUE)
    coords$label.r <- convertWidth(label.r, "native", valueOnly = TRUE)
    coords$label.size <- label.size
    coords$alignment <- alignment
    coords$min_distance <- min_distance


    rect_grobs <- lapply(1:nrow(coords), function(i) {
      grid::roundrectGrob(
        x = unit(coords$x[i], "native"),
        y = unit(coords$y[i], "native"),
        width = unit(0.1, "npc"),
        height = unit(0.1, "npc"),
        just = "center",
        r = unit(coords$label.r[i], "native"),
        gp = grid::gpar(
          col = coords$colour[i],
          fill = scales::alpha(coords$fill[i], coords$alpha[i]),
          lwd = coords$label.size[i] * .pt
        )
      )
    })

    text_grobs <- lapply(1:nrow(coords), function(i) {
      grid::textGrob(
        coords$label[i],
        x = unit(coords$x[i], "native"),
        y = unit(coords$y[i], "native"),
        just = "center",
        gp = grid::gpar(
          col = coords$colour[i],
          fontsize = coords$size[i] * .pt,
          fontfamily = coords$family[i],
          fontface = coords$fontface[i],
          lineheight = coords$lineheight[i]
        )
      )
    })

    grobs <- mapply(function(r, t) grid::gTree(children = grid::gList(r, t)), 
                   rect_grobs, text_grobs)
    class(grobs) <- "gList"
    ggname("geom_aligned_boxes", grid::grobTree(children = grobs))
  },

   pre_process = function(g, g.data, ...) {
    # This ensures our geom is identified as "aligned_boxes" in JS
    g$geom <- "aligned_boxes"
    return(list(g = g, g.data = g.data))
  },
  
  draw_key = draw_key_label
)