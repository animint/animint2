library(testthat)
library(animint2)
library(XML)

context("Bootstrap Sampling Animation renderer")

set.seed(123)
n <- 30
max_iterations <- 10

original_data <- rnorm(n, mean = 10, sd = 2)
original_df <- data.frame(
  id = 1:n,
  val = original_data,
  type = "Original",
  iteration = 0L,
  y = 1
)

boot_samples <- data.frame()
boot_means <- data.frame()

for (i in 1:max_iterations) {
  sample_vals <- sample(original_data, n, replace = TRUE)
  boot_samples <- rbind(
    boot_samples,
    data.frame(
      id = 1:n,
      val = sample_vals,
      type = "Bootstrap",
      iteration = as.integer(i),
      y = 1
    )
  )
  boot_means <- rbind(
    boot_means,
    data.frame(
      iteration = as.integer(i),
      mean_val = mean(sample_vals)
    )
  )
}

# Here I am combining original and bootstrap points

combined_df <- rbind(original_df, boot_samples)
combined_df$iteration <- as.integer(combined_df$iteration)
boot_means$iteration <- as.integer(boot_means$iteration)

viz <- list(
  bootstrapPlot = ggplot() +
    geom_point(
      data = combined_df,
      aes(x = val, y = y, colour = type),
      showSelected = c("iteration", "type"),
      clickSelects = "type",
      size = 3
    ) +
    geom_point(
      data = boot_means,
      aes(x = mean_val, y = 1.2),
      showSelected = "iteration",
      size = 4,
      colour = "red"
    ) +
    labs(
      title = "Bootstrap Sampling Animation",
      x = "Value", y = "Distribution"
    ) +
    theme_animint(width = 700, height = 400),
  time = list(variable = "iteration", ms = 1000),
  first = list(
    iteration = 1,                 
    type = c("Original", "Bootstrap")   # both types are initially selected
  )
)

info <- animint2HTML(viz)

# ---- Tests ----

test_that("Render animation correctly", {
  expect_true(!is.null(info))
})

test_that("HTML contains SVG", {
  page <- getHTML()
  expect_true(grepl("<svg", saveXML(page)))
})

test_that("Plot title exists", {
  page <- getHTML()
  expect_true(grepl("Bootstrap Sampling Animation", saveXML(page)))
})

test_that("Initial points visible (bootstrap + mean)", {
  page <- getHTML()
  pts <- getNodeSet(
    page,
    '//g[starts-with(@class, "geom") and contains(@class, "point")]//circle'
  )
  # n bootstrap points + 1 mean point
  expect_equal(length(pts), n + 1)
})

test_that("Clicking 'Bootstrap' legend hides bootstrap points", {
  clickID("plot_bootstrapPlot_type_variable_Bootstrap")
  Sys.sleep(0.5)
  
  page <- getHTML()
  pts <- getNodeSet(
    page,
    '//g[starts-with(@class, "geom") and contains(@class, "point")]//circle'
  )
  expect_equal(length(pts), 1)
})

test_that("Clicking 'Original' legend does not affect bootstrap points (original not visible)", {
  clickID("plot_bootstrapPlot_type_variable_Bootstrap")
  Sys.sleep(0.5)
  
  clickID("plot_bootstrapPlot_type_variable_Original")
  Sys.sleep(0.5)
  
  page <- getHTML()
  pts <- getNodeSet(
    page,
    '//g[starts-with(@class, "geom") and contains(@class, "point")]//circle'
  )
  #  n bootstrap points + 1 mean point
  expect_equal(length(pts), n + 1)
})
