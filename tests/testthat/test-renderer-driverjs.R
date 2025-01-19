library(animint2)
library(testthat)
library(XML)

data <- data.frame(
  year = rep(2000:2005, each = 3),
  country = rep(c("USA", "Canada", "Mexico"), times = 6),
  life_expectancy = c(78, 80, 76, 79, 81, 77, 80, 82, 78, 81, 83, 79, 82, 84, 80, 83, 85, 81),
  fertility_rate = c(1.8, 1.7, 2.1, 1.9, 1.8, 2.2, 1.9, 1.8, 2.3, 2.0, 1.9, 2.4, 2.1, 2.0, 2.5, 2.2, 2.1, 2.6),
  population = c(300, 100, 150, 310, 110, 160, 320, 120, 170, 330, 130, 180, 340, 140, 190, 350, 150, 200)
)

my_plot <- list(
  pointPlot = ggplot(data, aes(x = life_expectancy, y = fertility_rate)) +
    animint2::geom_point(
      aes(size = population, color = country),
      title = "Life Expectancy Threshold",
      help = "Each point represents life expectancy and fertility rate for a given country.",
      showSelected = "year",
      clickSelects = "country"
    ) +
    labs(title = "Life Expectancy vs. Fertility Rate", x = "Life Expectancy", y = "Fertility Rate"),
  vlinePlot = ggplot(data, aes(x = life_expectancy, y = fertility_rate)) +
    animint2::geom_vline(
      xintercept = 80,
      linetype = "dashed",
      color = "red",
      title = "Life Expectancy Threshold",
      help = "Vertical line represents a life expectancy threshold of 80."
    )
)

map <- animint2HTML(my_plot)
count_elements <- function(html_content, xpath) {
  html_text <- if (inherits(html_content, "XMLInternalDocument")) {
    saveXML(html_content)
  } else {
    as.character(html_content)
  }
  length(getNodeSet(htmlParse(html_text, asText = TRUE), xpath))
}

test_that("Check pixel ranges for geom1_point_pointPlot", {
  info <- list(html_updated1 = getHTML())
  no_updates_ranges1 <- get_pixel_ranges(info$html_updated1, "geom1_point_pointPlot")
  expect_gt(length(no_updates_ranges1$x), 0, "Expected at least one x value in pixel ranges for geom1_point_pointPlot")
  expect_gt(length(no_updates_ranges1$y), 0, "Expected at least one y value in pixel ranges for geom1_point_pointPlot")
})

test_that("Check if the title 'Life Expectancy vs. Fertility Rate' is present", {
  title_count <- count_elements(
    getHTML(),
    "//*[contains(text(), 'Life Expectancy vs. Fertility Rate')]"
  )
  expect_gt(title_count, 0, "Expected the text 'Life Expectancy vs. Fertility Rate' to be present in the plot title.")
})

test_that("Check first element of tour after clicking 'start_tour'", {
  clickID("start_tour")
  first_element_count <- count_elements(
    getHTML(),
    "//div[contains(@class, 'driver-popover-title')][contains(text(), 'Life Expectancy Threshold')]"
  )
  expect_gt(first_element_count, 0, "Expected at least one 'Geom1_point_pointPlot' element in the tour popup")
})

test_that("Check second element of tour after clicking 'Next'", {
  runtime_evaluate_helper(
    class_name = "driver-popover-next-btn",
    list_num = 0,
    dispatch_event = TRUE
  )
  second_element_count <- count_elements(
    getHTML(),
    "//div[contains(@class, 'driver-popover-title') and contains(text(), 'Life Expectancy Threshold')]"
  )
  expect_gt(second_element_count, 0, "Expected 'Geom2_vline_vlinePlot' element after clicking Next")
})
