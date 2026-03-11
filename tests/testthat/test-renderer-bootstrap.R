library(testthat)
library(animint2)
library(XML)

context("Bootstrap Sampling Animation renderer")

tests_init()

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
    theme_animint(
      width = 500,    
      height = 300   
    ) +
    theme(
      legend.position = "bottom",  
      legend.direction = "horizontal"
    ),
  time = list(variable = "iteration", ms = 1000),
  first = list(
    iteration = 1,                 
    type = c("Original", "Bootstrap")   # both types are initially selected
  )
)

info <- animint2HTML(viz)

get_point_count <- function(){
  page <- getHTML()
  nodes <- getNodeSet(
    page,
    '//g[starts-with(@class, "geom") and contains(@class, "point")]//circle'
  )
  length(nodes)
}

# ---- Tests ----

test_that("Render animation correctly", {
  expect_true(!is.null(info))
})

test_that("HTML contains SVG", {
  expect_true(grepl("<svg", saveXML(getHTML())))
})

test_that("Plot title exists", {
  expect_true(grepl("Bootstrap Sampling Animation", saveXML(getHTML())))
})

# ---------------- INTERACTIVE TESTS ----------------

test_that("Initial points visible (bootstrap + mean)", {
  count <- get_point_count()
  cat(sprintf("Result: %d points remaining \n", count))
  expect_equal(count, n + 1)
})

test_that("showSelected: changing iteration via selector widget", {
  clickID("iteration___5") 
  Sys.sleep(5)
  count <- get_point_count()
  cat(sprintf("Result: %d point(s) remaining \n", count))
  expect_equal(count, n + 1)
})

test_that("clickSelects: Legend click hides bootstrap points", {
  clickID("plot_bootstrapPlot_type_variable_Bootstrap")
  Sys.sleep(5)
  count <- get_point_count()
  cat(sprintf("Result: %d point remaining \n", count))
  expect_equal(count, 1)
})