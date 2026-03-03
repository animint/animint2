library(testthat)
library(animint2)
library(XML)

# Starts the browser for remote testing
tests_init()
Sys.sleep(2)

# Setup random seed and parameters
set.seed(123)
n <- 30
max_iterations <- 10

# Creating the original baseline data
original_data <- rnorm(n, mean = 10, sd = 2)

original_df <- data.frame(
  id = 1:n,
  val = original_data,
  type = "Original",
  iteration = 0,
  y = 1
)

# Initializing storage for bootstrap results
boot_samples <- data.frame()
boot_means <- data.frame()

# Running the bootstrap resampling loop
for (i in 1:max_iterations) {
  
  sample_vals <- sample(original_data, n, replace = TRUE)
  
  # Storing individual points for this iteration
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
  
  # Storing the mean for this iteration
  boot_means <- rbind(
    boot_means,
    data.frame(
      iteration = as.integer(i),
      mean_val = mean(sample_vals)
    )
  )
}

# Merging datasets and fix data types
combined_df <- rbind(original_df, boot_samples)
combined_df$iteration <- as.integer(combined_df$iteration)
boot_means$iteration <- as.integer(boot_means$iteration)

# Building the interactive visualization object
viz <- list(
  bootstrapPlot = ggplot() +
    geom_point(
      data = combined_df,
      aes(x = val, y = y, colour = type),
      showSelected = "iteration",
      clickSelects = "type"
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
    ),
  width = list(bootstrapPlot = 400), 
  height = list(bootstrapPlot = 700),
  # Seting animation timing
  time = list(variable = "iteration", ms = 1000)
)

# Rendering to HTML for testing
info <- animint2HTML(viz)

# --- Test Suite ---

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

test_that("Points visible initially", {
  page <- getHTML()
  pts <- getNodeSet(
    page,
    '//g[contains(@class,"geom1_point_bootstrapPlot")]//circle'
  )
  expect_gt(length(pts), 0)
})

# Verifying that clicking 'Bootstrap' legend works
test_that("Bootstrap points disappear after click", {
  clickID("plot_bootstrapPlot_type_variable_Bootstrap")
  Sys.sleep(0.5)
  
  after_click <- getHTML()
  pts <- getNodeSet(
    after_click,
    '//g[contains(@class,"geom1_point_bootstrapPlot")]//circle'
  )
  expect_equal(length(pts), 0)
})

# Verifying that clicking 'Original' legend works
test_that("Original points disappear after click", {
  clickID("plot_bootstrapPlot_type_variable_Original")
  Sys.sleep(0.5)
  
  after_click2 <- getHTML()
  pts <- getNodeSet(
    after_click2,
    '//g[contains(@class,"geom1_point_bootstrapPlot")]//circle'
  )
  expect_equal(length(pts), 0)
})