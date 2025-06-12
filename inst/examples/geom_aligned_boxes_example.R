library(animint2)
set.seed(42)
city_names <- c("Delhi", "Mumbai", "Bangalore", "Chennai", "Kolkata")
n_timepoints <- 10

city_data <- do.call(rbind, lapply(city_names, function(city) {
  data.frame(
    Time = 1:n_timepoints,
    Temperature = cumsum(rnorm(n_timepoints, mean = 0.5, sd = 2)) + runif(1, 20, 30),
    City = city
  )
}))

# Manually override the final y-values of 3 cities to force collision
city_data$Temperature[city_data$City == "Delhi" & city_data$Time == n_timepoints] <- 40
city_data$Temperature[city_data$City == "Mumbai" & city_data$Time == n_timepoints] <- 40
city_data$Temperature[city_data$City == "Bangalore" & city_data$Time == n_timepoints] <- 40

# Let Chennai and Kolkata be different
city_data$Temperature[city_data$City == "Chennai" & city_data$Time == n_timepoints] <- 5
city_data$Temperature[city_data$City == "Kolkata" & city_data$Time == n_timepoints] <- 10

label_data <- city_data[city_data$Time == n_timepoints, ]
label_data$label <- label_data$City

p <- ggplot() +
  geom_line(
    data = city_data,
    aes(x = Time, y = Temperature, color = City, group = City),
    size = 1.2
  ) +
  geom_aligned_boxes(
    data = label_data,
    aes(x = Time, y = Temperature, label = label, fill = City),
    alignment = "vertical",
    showSelected = "City"
  ) +
  ggtitle("Temperature Trends in Indian Cities") + 
  xlab("Time (Day)") + ylab("Temperature (°C)")

viz <- list(cityTemp = p)
animint2dir(viz, "smart_aligned_labels")