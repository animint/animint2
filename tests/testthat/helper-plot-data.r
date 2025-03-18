library(data.table)
# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)

  dt <- rbindlist(pieces$data, idcol = "data_id", fill = TRUE)

  result <- dt[, {
    scales <- panel_scales(pieces$panel, PANEL[1])
    details <- plot$coordinates$train(scales)
    transformed_data <- plot$coordinates$transform(.SD, details)
    transformed_data
  }, by = .(data_id, PANEL)]

  # Split the result back into a list of data.tables
  split(result, by = "data_id", keep.by = FALSE)
}

pranges <- function(plot) {
  panels <- ggplot_build(plot)$panel

  x_ranges <- lapply(panels$x_scales, function(scale) scale$get_limits())
  y_ranges <- lapply(panels$y_scales, function(scale) scale$get_limits())
  
  npscales <- plot$scales$non_position_scales()
  npranges <- lapply(npscales$scales$scales, function(scale) scale$get_limits())

  c(list(x = x_ranges, y = y_ranges), npranges)
}
