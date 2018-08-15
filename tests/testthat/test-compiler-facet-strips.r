context("Facet Strips")

strip_layout <- function(p) {
  data <- a_plot_build(p)
  plot <- data$plot
  panel <- data$panel
  data <- data$data
  a_theme <- plot_a_theme(plot)

  a_geom_grobs <- Map(function(l, d) l$draw_geom(d, panel, plot$coordinates),
    plot$layers, data)

  a_facet <- a_facet_render(plot$a_facet, panel, plot$coordinates, a_theme, a_geom_grobs)
  layout <- a_facet$layout
  strip_layout <- layout[grepl("^strip", layout$name), 1:4]
  as.list(strip_layout)
}

p <- a_plot(mtcars, a_aes(disp, drat)) + a_geom_point()


test_that("a_facet_wrap() builds correct output", {
  wrap <- p + a_facet_wrap(~cyl)

  wrap_expected <- list(
    t = c(1, 1, 1),
    l = c(2, 5, 8),
    b = c(1, 1, 1),
    r = c(2, 5, 8)
  )

  expect_equal(strip_layout(wrap), wrap_expected)
})

test_that("a_facet_wrap() switches to 'x'", {
  wrap_x <- p + a_facet_wrap(~cyl, switch = "x")

  wrap_x_expected <- list(
    t = c(3, 3, 3),
    l = c(2, 5, 8),
    b = c(3, 3, 3),
    r = c(2, 5, 8)
  )

  expect_equal(strip_layout(wrap_x), wrap_x_expected)
})

test_that("a_facet_wrap() switches to 'y'", {
  wrap_y <- p + a_facet_wrap(~cyl, switch = "y")

  wrap_y_expected <- list(
    t = c(1, 1, 1),
    l = c(1, 5, 9),
    b = c(1, 1, 1),
    r = c(1, 5, 9)
  )

  expect_equal(strip_layout(wrap_y), wrap_y_expected)
})


test_that("a_facet_grid() builds correct output", {
  grid <- p + a_facet_grid(~cyl)

  grid_expected <- list(
    t = c(1, 1, 1),
    l = c(2, 4, 6),
    b = c(1, 1, 1),
    r = c(2, 4, 6)
  )

  expect_equal(strip_layout(grid), grid_expected)
})

test_that("a_facet_grid() switches to 'x'", {
  grid_x <- p + a_facet_grid(am ~ cyl, switch = "x")

  grid_x_expected <- list(
    t = c(1, 3, 5),
    l = c(7, 7, 2),
    b = c(1, 3, 6),
    r = c(7, 7, 6)
  )

  expect_equal(strip_layout(grid_x), grid_x_expected)
})

test_that("a_facet_grid() switches to 'y'", {
  grid_y <- p + a_facet_grid(am ~ cyl, switch = "y")

  grid_y_expected <- list(
    t = c(1, 1, 1, 2),
    l = c(4, 6, 8, 1),
    b = c(1, 1, 1, 4),
    r = c(4, 6, 8, 2)
  )

  expect_equal(strip_layout(grid_y), grid_y_expected)
})

test_that("a_facet_grid() switches to both 'x' and 'y'", {
  grid_xy <- p + a_facet_grid(am ~ cyl, switch = "both")

  grid_xy_expected <- list(
    t = c(1, 5),
    l = c(1, 4),
    b = c(3, 6),
    r = c(2, 8)
  )

  expect_equal(strip_layout(grid_xy), grid_xy_expected)
})
