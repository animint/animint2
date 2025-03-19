library(data.table)
# Transform the data as the coordinate system does

cdata <- function(plot) {
  pieces <- ggplot_build(plot)
  
  # Process each data frame in pieces$data
  DT <- data.table(index = seq_along(pieces$data), data = pieces$data)
  
  DT[, {
    d <- as.data.table(data[[1]])
    d[, {
      scales <- panel_scales(pieces$panel, PANEL)
      details <- plot$coordinates$train(scales)
      plot$coordinates$transform(.SD, details)
    }, by = .(PANEL)]
  }, by = index]
}


pranges <- function(plot) {
  panels <- ggplot_build(plot)$panel

  x_ranges <- lapply(panels$x_scales, function(scale) scale$get_limits())
  y_ranges <- lapply(panels$y_scales, function(scale) scale$get_limits())
  
  npscales <- plot$scales$non_position_scales()
  npranges <- lapply(npscales$scales$scales, function(scale) scale$get_limits())

  c(list(x = x_ranges, y = y_ranges), npranges)
}
