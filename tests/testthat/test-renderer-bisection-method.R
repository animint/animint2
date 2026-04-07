library(testthat)
library(animint2)
library(XML)

context("Bisection Method Animation renderer")

tests_init()

# ----------- DATA SETUP -----------

set.seed(42)
max_iterations <- 10

# f(x) = x^3 - x - 2
f <- function(x) x^3 - x - 2

a_init <- 1.0
b_init <- 2.0

bisection_data <- data.frame()
interval_data  <- data.frame()

a <- a_init
b <- b_init

for (i in 1:max_iterations) {
  mid      <- (a + b) / 2
  f_mid    <- f(mid)
  f_a      <- f(a)
  
  bisection_data <- rbind(
    bisection_data,
    data.frame(
      iteration = as.integer(i),
      midpoint  = mid,
      f_mid     = f_mid,
      side      = ifelse(f_mid * f_a < 0, "left", "right")
    )
  )
  
  interval_data <- rbind(
    interval_data,
    data.frame(
      iteration = as.integer(i),
      a         = a,
      b         = b,
      y         = 0
    )
  )
  
  if (f_mid * f_a < 0) {
    b <- mid
  } else {
    a <- mid
  }
}

bisection_data$iteration <- as.integer(bisection_data$iteration)
interval_data$iteration  <- as.integer(interval_data$iteration)

# x values for the curve
curve_df <- data.frame(
  x = seq(0.8, 2.2, length.out = 200),
  y = f(seq(0.8, 2.2, length.out = 200)),
  iteration = 1L
)

# ----------- VISUALIZATION -----------

#static blue curve
viz <- list(
  bisectionPlot = ggplot() +
    geom_line(
      data  = curve_df,
      aes(x = x, y = y),
      colour = "steelblue",
      size   = 1
    ) +
    geom_vline(
      data = interval_data,
      aes(xintercept = a),
      showSelected = "iteration",
      colour    = "green",
      linetype  = "dashed",
      size      = 1
    ) +
    geom_vline(
      data = interval_data,
      aes(xintercept = b),
      showSelected = "iteration",
      colour   = "orange",
      linetype = "dashed",
      size     = 1
    ) +
    geom_point(
      data = bisection_data,
      aes(x = midpoint, y = f_mid, colour = side),
      showSelected  = "iteration",
      clickSelects  = "side",
      size = 5
    ) +
    geom_hline(yintercept = 0, colour = "gray50", linetype = "dotted") +
    labs(
      title = "Bisection Method Animation",
      x     = "x",
      y     = "f(x)"
    ) +
    theme_animint(width = 550, height = 350) +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal"
    ),
  
  time  = list(variable = "iteration", ms = 1000),
  first = list(
    iteration = 1L,
    side      = c("left", "right")
  )
)

info <- animint2HTML(viz)

# ----------- HELPER(fetches live DOM from chrome) -----------

get_point_count <- function() {
  page  <- getHTML()
  nodes <- getNodeSet(
    page,
    '//g[starts-with(@class, "geom") and contains(@class, "point")]//circle'
  )
  length(nodes)
}

# ----------- TESTS -----------

test_that("Render animation correctly", {
  expect_true(!is.null(info))
})

test_that("HTML contains SVG", {
  expect_true(grepl("<svg", saveXML(getHTML())))
})

test_that("Plot title exists", {
  expect_true(grepl("Bisection Method Animation", saveXML(getHTML())))
})

# ----------- INTERACTIVE TESTS -----------

test_that("Initial midpoint visible at iteration 1", {
  count <- get_point_count()
  cat(sprintf("Result: %d point(s) visible\n", count))
  expect_equal(count, 1)
})

test_that("showSelected: switching to iteration 5 still shows 1 midpoint", {
  clickID("iteration___5")
  Sys.sleep(5)
  count <- get_point_count()
  cat(sprintf("Result: %d point(s) visible\n", count))
  expect_equal(count, 1)
})

test_that("clickSelects: deselecting a side hides that midpoint", {
  # reset to iteration 1 first
  clickID("iteration___1")
  Sys.sleep(3)
  
  before <- get_point_count()
  cat(sprintf("Before deselect: %d point(s)\n", before))
  
  # The midpoint at iteration 1 is on the "right" side — deselect it
  clickID("plot_bisectionPlot_side_variable_right")
  Sys.sleep(5)
  
  after <- get_point_count()
  cat(sprintf("After deselect: %d point(s)\n", after))
  expect_equal(after, 0)
})
