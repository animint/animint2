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

# Plot 2 : Collisions with axes and other boxes at the same time 
library(nlme)
library(dplyr)
data(BodyWeight, package = "nlme")
# Extracting the last point of each rat's trajectory
label_data <- BodyWeight %>%
  group_by(Rat) %>%
  filter(Time == max(Time)) %>%
  ungroup() %>%
  mutate(label = as.character(Rat))

viz2 <- list(
  bodyPlot = ggplot() +
    geom_line(aes(x = Time, y = weight, group = Rat, colour = Rat),
              data = BodyWeight) +
    geom_aligned_boxes(aes(x = Time, y = weight, label = label, fill = Rat),
                       data = label_data) +
    facet_wrap(~Diet, nrow = 1) +
    ggtitle("Rat body weight over time by diet") +
    xlab("Time (days)") +
    ylab("Body Weight (grams)")
)

# Render to directory
animint2dir(viz2, "bodyweight-aligned-boxes")

# Plot 3 : Collisions from all directions
# Simulating 15 points arranged close enough to test for all-direction collisions
set.seed(42)
label_data <- data.frame(
  x = c(1, 2, 3, 4, 5, 3.5, 2.5, 3, 4, 1.5, 2, 4.5, 3, 2, 5),
  y = c(1, 2, 3, 4, 5, 2.5, 3.5, 4, 1, 4.5, 2, 3, 3.2, 2.8, 1.2),
  label = paste("Label", 1:15)
)

# color for visibility
label_data$group <- factor(1:15)

viz3 <- list(
  overlapTest = ggplot() +
    geom_aligned_boxes(aes(x, y, label = label, fill = group), data = label_data) +
    ggtitle("Overlap Test: Aligned Boxes") +
    xlab("X") +
    ylab("Y")
)

animint2dir(viz3, "overlap-aligned-boxes")