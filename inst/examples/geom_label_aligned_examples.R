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

# Create label data for the aligned labels at final time point
label_data <- line_data[line_data$Time == n_timepoints, ]
label_data$label <- label_data$Label

p <- ggplot() +
  geom_line(
    data = line_data,
    aes(x = Time, y = Value, color = Label, group = Label),
    size = 1.2
  ) +
  geom_label_aligned(
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
    geom_label_aligned(aes(x = Time, y = weight, label = label, fill = Rat),
                       data = label_data) +
    facet_wrap(~Diet, nrow = 1) +
    ggtitle("Rat body weight over time by diet") +
    xlab("Time (days)") +
    ylab("Body Weight (grams)")
)

# Render to directory
animint2dir(viz2, "bodyweight-label-aligned")

# Example 3: World Bank Data with Interactive Aligned Labels
library(dplyr)
data(WorldBank, package = "animint2")

# subset of countries
tracked_countries <- c("United States", "Vietnam", "India", "China", "Brazil",
                      "Nigeria", "Germany", "South Africa")

# Filter WorldBank data
wb <- subset(WorldBank, country %in% tracked_countries) %>%
  mutate(
    year = as.integer(year),
    group = country
  )

# Label data for aligned labels
label_data <- wb %>%
  mutate(label = country)

# Text data for year display
year_text_data <- data.frame(
  year = unique(wb$year),
  label = unique(wb$year)
)

viz3 <- list(
  worldbankAnim = ggplot() +
    geom_point(
      data = wb,
      aes(x = fertility.rate, y = life.expectancy, color = group, key = country),
      size = 8,
      showSelected = "year",
      clickSelects = "country"
    ) +
    geom_label_aligned(
      data = label_data,
      aes(x = fertility.rate, y = life.expectancy, label = label, fill = group, key = country),
      alignment = "vertical", color = "#ffffd1", label_r = "9",
      showSelected = "year",
      clickSelects = "country"
    ) +
    make_text(year_text_data, x = 1, y = 82, label = "label") +
    ggtitle("Life Expectancy vs Fertility Rate") +
    xlab("Fertility Rate") +
    ylab("Life Expectancy"),

  timeSeries = ggplot() +
    geom_line(
      data = wb,
      aes(x = year, y = life.expectancy, group = country, color = group),
      size = 1.5,
      showSelected = "country"
    ) +
    geom_point(
      data = wb,
      aes(x = year, y = life.expectancy, color = group),
      showSelected = "country",
      size = 2
    ) +
    ggtitle("Life Expectancy Over Time (Selected Country)") +
    xlab("Year") +
    ylab("Life Expectancy"),

  time = list(variable = "year", ms = 1000),
  duration = list(year = 800),
  first = list(year = min(wb$year)),
  selector.types = list(country = "multiple")
)

animint2dir(viz3, "worldbank-label-aligned")