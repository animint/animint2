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
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_aligned_boxes()
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
    colour = "black", fill = "grey35", size = 0.5, 
    linetype = 1, alpha = 0.5, family = "", 
    fontface = 1, lineheight = 1.2
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

    # Optimize positions if needed
    if (alignment == "vertical") {
      coords$y <- optimize_vertical_positions(coords$y, rep(0.1, nrow(coords)), min_distance)
    } else {
      coords$x <- optimize_horizontal_positions(coords$x, rep(0.1, nrow(coords)), min_distance)
    }

    # Add parameters for JavaScript
    coords$label.padding <- convertWidth(label.padding, "native", valueOnly = TRUE)
    coords$label.r <- convertWidth(label.r, "native", valueOnly = TRUE)
    coords$label.size <- label.size

    # Create grobs for static plot
    rect_grobs <- lapply(1:nrow(coords), function(i) {
      row <- coords[i, , drop = FALSE]
      grid::roundrectGrob(
        x = unit(row$x, "native"),
        y = unit(row$y, "native"),
        width = unit(0.1, "native"),  # Will be calculated in JS
        height = unit(0.1, "native"), # Will be calculated in JS
        just = c(0.5, 0.5),
        r = unit(row$label.r, "native"),
        gp = grid::gpar(
          col = row$colour,
          fill = scales::alpha(row$fill, row$alpha),
          lwd = row$label.size * .pt
        )
      )
    })

    text_grobs <- lapply(1:nrow(coords), function(i) {
      row <- coords[i, , drop = FALSE]
      grid::textGrob(
        row$label,
        x = unit(row$x, "native"),
        y = unit(row$y, "native"),
        # just = c(row$hjust, row$vjust),
        just = c(0.5, 0.5),
        gp = grid::gpar(
          col = row$colour,
          fontsize = row$size * .pt,
          fontfamily = row$family,
          fontface = row$fontface,
          lineheight = row$lineheight
        )
      )
    })

    grobs <- mapply(function(rect, text) grid::gTree(children = grid::gList(rect, text)),
                    rect_grobs, text_grobs, SIMPLIFY = FALSE)
    class(grobs) <- "gList"
    ggname("geom_aligned_boxes", grid::grobTree(children = grobs))
  },

  draw_key = draw_key_label
)

# Helper function to optimize vertical positions using quadratic programming
optimize_vertical_positions <- function(y, heights, min_distance) {
  n <- length(y)
  if (n <= 1) return(y)
  Dmat <- diag(n)
  dvec <- y
  Amat <- matrix(0, nrow = n * (n-1)/2, ncol = n)
  bvec <- numeric(n * (n-1)/2)
  k <- 1
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      Amat[k, i] <- 1
      Amat[k, j] <- -1
      bvec[k] <- (heights[i] + heights[j])/2 + min_distance
      k <- k + 1
    }
  }
  result <- quadprog::solve.QP(Dmat, dvec, t(Amat), bvec)
  return(result$solution)
}

# Helper function to optimize horizontal positions using quadratic programming
optimize_horizontal_positions <- function(x, widths, min_distance) {
  n <- length(x)
  if (n <= 1) return(x)
  Dmat <- diag(n)
  dvec <- x
  Amat <- matrix(0, nrow = n * (n-1)/2, ncol = n)
  bvec <- numeric(n * (n-1)/2)
  k <- 1
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      Amat[k, i] <- 1
      Amat[k, j] <- -1
      bvec[k] <- (widths[i] + widths[j])/2 + min_distance
      k <- k + 1
    }
  }
  result <- quadprog::solve.QP(Dmat, dvec, t(Amat), bvec)
  return(result$solution)
}