library(data.table)
# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)

  lapply(pieces$data, function(d) {
    d <- as.data.table(d)  
    d[, {
      scales <- panel_scales(pieces$panel, .SD$PANEL[1])
      details <- plot$coordinates$train(scales)
      transformed_data <- plot$coordinates$transform(.SD, details)
      transformed_data
    }, by = .(PANEL)]
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
