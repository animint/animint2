acontext("TestWorldBankTwoLayers")

data(WorldBank, package="animint2")

# Filter the data for specific years
WorldBank1975 <- subset(WorldBank, year == 1975)
WorldBankBefore1975 <- subset(WorldBank, 1970 <= year & year <= 1975)

# Define income levels and colors
income.colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
WorldBank$income <- factor(WorldBank$income, 
                           levels = c("High income: nonOECD", "High income: OECD", 
                                      "Upper middle income", "Lower middle income", 
                                      "Low income", "Not classified"))
names(income.colors) <- levels(WorldBank$income)

# Create the scatter plot with one layer
scatter <- ggplot() +
  scale_color_manual(values = income.colors) +
  geom_point(
    mapping = aes(x = life.expectancy, y = fertility.rate, color = income),
    data = WorldBank1975
  )

# Add the second layer for paths
two.layers <- scatter +
  geom_path(
    aes(
      x = life.expectancy,
      y = fertility.rate,
      color = income,
      group = country
    ),
    data = WorldBankBefore1975
  )

# Create an interactive plot
scatter_plot <- list(plot = two.layers)
animint(scatter_plot)

# Test that the visualization generates without warnings or errors
test_that("worldbank two-layers visualization generated without any warnings or errors", {
  expect_no_warning({
    info <- animint2HTML(scatter_plot)
  })
})