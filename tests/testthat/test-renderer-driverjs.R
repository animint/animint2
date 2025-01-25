library(animint2)
library(testthat)
library(XML)

wb_data <- data.frame(
  year = rep(2000:2005, each = 3),
  country = rep(c("USA", "Canada", "Mexico"), times = 6),
  life_expectancy = c(78, 80, 76, 79, 81, 77, 80, 82, 78, 81, 83, 79, 82, 84, 80, 83, 85, 81),
  fertility_rate = c(1.8, 1.7, 2.1, 1.9, 1.8, 2.2, 1.9, 1.8, 2.3, 2.0, 1.9, 2.4, 2.1, 2.0, 2.5, 2.2, 2.1, 2.6),
  population = c(300, 100, 150, 310, 110, 160, 320, 120, 170, 330, 130, 180, 340, 140, 190, 350, 150, 200)
)

wb_viz <- list(
  pointPlot = ggplot(wb_data, aes(x = life_expectancy, y = fertility_rate)) +
    geom_point(
      aes(size = population, color = country),
      title = "One country",
      help = "Each point represents life expectancy and fertility rate for a given country.",
      showSelected = "year",
      clickSelects = "country"
    ) +
    labs(title = "Life Expectancy vs. Fertility Rate", x = "Life Expectancy", y = "Fertility Rate"),
  vlinePlot = ggplot(wb_data, aes(x = life_expectancy, y = fertility_rate)) +
    geom_vline(
      xintercept = 80,
      linetype = "dashed",
      color = "red"
    )
)
info <- animint2HTML(wb_viz)

djs.list <- driverjs_get(info$html)
test_that("World Bank no title nor description initially", {
  expect_identical(djs.list$title, list())
  expect_identical(djs.list$description, list())
})

djs.list.start <- driverjs_start()
test_that("World Bank first element of Start Tour", {
  expect_identical(djs.list.start$title, list(
    text="One country",
    .attrs=c(
      class="driver-popover-title",
      style="display: block;")))
  expect_identical(djs.list.start$description, list(
    text = "Each point represents life expectancy and fertility rate for a given country.",
    br = NULL,
    text = "Data are shown for the current selection of: year,country",
    br = NULL,
    text = "Click to select: country",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;")))
})

djs.list.next <- driverjs_next()
test_that("World Bank tour after clicking Next", {
  expect_identical(djs.list.next$title, list(
    text = "geom2_vline_vlinePlot",
    .attrs = c(
      class = "driver-popover-title",
      style = "display: block;")))
  expect_identical(djs.list.next$description, list(
    text = "No interactions available",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;")))
})

viz <- animint(
  pointRect=ggplot()+
    geom_line(aes(
      x = year,
      y = fertility_rate,
      group=country,
      color = country),
      data=wb_data,
      )+
    make_tallrect(wb_data, "year"),
  duration=list(year=2000)
)
info <- animint2HTML(viz)

djs.list <- driverjs_get(info$html)
test_that("driver tallrect no title nor description initially", {
  expect_identical(djs.list$title, list())
  expect_identical(djs.list$description, list())
})

djs.list.start <- driverjs_start()
test_that("driver tallrect Start Tour", {
  expect_identical(djs.list.start$title, list(
    text="geom1_line_pointRect",
    .attrs=c(
      class="driver-popover-title",
      style="display: block;")))
  expect_identical(djs.list.start$description, list(
    text = "Data are shown for the current selection of: country",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;")))
})

djs.list.next <- driverjs_next()
test_that("driver tallrect after clicking Next", {
  expect_identical(djs.list.next$title, list(
    text = "geom2_tallrect_pointRect",
    .attrs = c(
      class = "driver-popover-title",
      style = "display: block;")))
  expect_identical(djs.list.next$description, list(
    text = "Data are shown for the current selection of: year",
    br=NULL,
    text = "Click to select: year",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;")))
})

sel_df <- data.frame(
  selectorName=sprintf("model_%d_size", 1:10),
  selectorValue=1:100)
sel_viz <- animint(
  points=ggplot()+
    geom_point(aes(
      selectorValue, selectorName),
      data=sel_df,
      size=5,
      clickSelects=c(selectorName="selectorValue"))
)
info <- animint2HTML(sel_viz)

djs.list <- driverjs_get(info$html)
test_that("driver sel no title nor description initially", {
  expect_identical(djs.list$title, list())
  expect_identical(djs.list$description, list())
})

djs.list.start <- driverjs_start()
test_that("driver sel Start Tour", {
  expect_identical(djs.list.start$title, list(
    text="geom1_point_points",
    .attrs=c(
      class="driver-popover-title",
      style="display: block;")))
  expect_identical(djs.list.start$description, list(
    text = "Click to select: one of: [model_1_size, model_2_size, ..., model_9_size, model_10_size]",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;")))
})
