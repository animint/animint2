acontext("geom_point_stroke")

stroke_in_R <- 5
viz <- animint(
  param=ggplot(mtcars, aes(
    wt, mpg)) +
    geom_point(
      shape = 21, colour = "black", fill = "white",
      size = 5, stroke = stroke_in_R),
  aes=ggplot(mtcars, aes(
    wt, mpg, stroke=cyl)) +
    geom_point(
      shape = 21, colour = "black", fill = "white", size = 5))

info <- animint2HTML(viz)

test_that("geom_point stroke param rendered with stroke-width", {
  stroke_vals <- getStyleValue(
    info$html,
    '//g[@class="geom1_point_param"]//circle', 
    "stroke-width")
  expect_equal(length(stroke_vals), length(mtcars$wt))
  stroke_vals_unique <- unique(stroke_vals)
  expect_equal(length(stroke_vals_unique), 1)
  stroke_width_val <- as.numeric(gsub("[^0-9]", "", stroke_vals_unique))
  expect_equal(stroke_width_val, stroke_in_R)
})

test_that("aes(stroke) works", {
  stroke_vals_aes <- getStyleValue(
    info$html,
    '//g[@class="geom2_point_aes"]//circle',
    "stroke-width")
  expect_equal(length(stroke_vals_aes), length(mtcars$wt))
  stroke_vals_unique_aes <- unique(stroke_vals_aes)
  expect_equal(length(stroke_vals_unique_aes), length(unique(mtcars$cyl)))
  stroke_width_vals <- as.numeric(gsub("[^0-9]", "", stroke_vals_unique_aes))
  expect_identical(sort(stroke_width_vals), sort(unique(mtcars$cyl)))
})
