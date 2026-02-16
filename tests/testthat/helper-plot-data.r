# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)

  lapply(pieces$data, function(d) {
    dt <- data.table::as.data.table(d)
    dt[, {
      scales <- panel_scales(pieces$panel, .BY$PANEL)
      details <- plot$coordinates$train(scales)
      # .SD excludes the grouping column (PANEL) by default, but transform might need it
      # or simply passthrough. We emulate ddply behavior by reconstructing it if needed
      # or leveraging the fact that transform typically operates on params.
      # To be safe and identical to ddply input:
      subset_d <- .SD
      subset_d$PANEL <- .BY$PANEL
      plot$coordinates$transform(subset_d, details)
    }, by = PANEL]
  })
}

pranges <- function(plot) {
  panels <- ggplot_build(plot)$panel

  x_ranges <- lapply(panels$x_scales, function(scale) scale$get_limits())
  y_ranges <- lapply(panels$y_scales, function(scale) scale$get_limits())


  npscales <- plot$scales$non_position_scales()
  npranges <- lapply(npscales$scales$scales, function(scale) scale$get_limits())


  c(list(x = x_ranges, y = y_ranges), npranges)
}
