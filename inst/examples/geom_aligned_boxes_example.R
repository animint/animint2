library(animint2)
set.seed(42)
# Create synthetic labels
label_names <- paste("Label", 1:5)
n_timepoints <- 10

line_data <- do.call(rbind, lapply(label_names, function(label) {
  data.frame(
    Time = 1:n_timepoints,
    Value = cumsum(rnorm(n_timepoints, mean = 0.5, sd = 2)) + runif(1, 20, 30),
    Label = label
  )
}))

# Manually override the final y-values of some labels to create overlaps
line_data$Value[line_data$Label == "Label 1" & line_data$Time == n_timepoints] <- 40
line_data$Value[line_data$Label == "Label 2" & line_data$Time == n_timepoints] <- 40
line_data$Value[line_data$Label == "Label 3" & line_data$Time == n_timepoints] <- 40

# Create label data for the aligned boxes at final time point
label_data <- line_data[line_data$Time == n_timepoints, ]
label_data$label <- label_data$Label

p <- ggplot() +
  geom_line(
    data = line_data,
    aes(x = Time, y = Value, color = Label, group = Label),
    size = 1.2
  ) +
  geom_aligned_boxes(
    data = label_data,
    aes(x = Time, y = Value, label = label, fill = Label),
    alignment = "vertical"
  ) +
  ggtitle("Synthetic Trends with Smart Aligned Labels") + 
  xlab("Time") + 
  ylab("Value")

viz <- list(syntheticTrend = p)
animint2dir(viz, "smart_aligned_labels")