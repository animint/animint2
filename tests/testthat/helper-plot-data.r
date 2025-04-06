# Transform the data as the coordinate system does
library(data.table)
# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)
  # Process each piece of data while maintaining panel structure
  result <- lapply(pieces$data, function(d) {
    dt <- as.data.table(d)

    # Explicitly group by PANEL and process each panel's data
    dt[, {
      scales <- panel_scales(pieces$panel, PANEL)
      details <- plot$coordinates$train(scales)
      as.data.table(plot$coordinates$transform(as.data.frame(.SD), details))
    }, by = PANEL]
  })

  return(result)
}

pranges <- function(plot) {
  panels <- ggplot_build(plot)$panel

  x_ranges <- lapply(panels$x_scales, function(scale) scale$get_limits())
  y_ranges <- lapply(panels$y_scales, function(scale) scale$get_limits())


  npscales <- plot$scales$non_position_scales()
  npranges <- lapply(npscales$scales$scales, function(scale) scale$get_limits())


  c(list(x = x_ranges, y = y_ranges), npranges)
}
