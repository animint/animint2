library(testthat)
library(XML)

# Newton-Raphson Method of root finding
f <- function(x) 5 * (x^3) - 7 * (x^2) - 40 * x + 100
f_prime <- function(x) 15 * (x^2) - 14 * x - 40

x0 <- 10
tolerance <- 1e-4
max_iterations <- 30

iterations_df <- data.frame(
  iteration = numeric(0),
  x = numeric(0),
  f_x = numeric(0),
  tangent_intercept = numeric(0)
)

for (i in 1:max_iterations) {
  f_x0 <- f(x0)
  f_prime_x0 <- f_prime(x0)
  
  # Calculating the next approximation
  x1 <- x0 - f_x0 / f_prime_x0
  
  iterations_df <- rbind(iterations_df, data.frame(
    iteration = i,
    x = x0,
    f_x = f_x0,
    tangent_intercept = x1
  ))
  
  if (abs(x1 - x0) < tolerance) {
    break
  }
  
  x0 <- x1
}

x_vals <- seq(floor(min(iterations_df$x)), ceiling(max(iterations_df$x)), length.out = 500)
function_data_df <- data.frame(x = x_vals, y = f(x_vals))

# Data frame for tangent lines
tangent_data_df <- iterations_df
tangent_data_df$slope <- f_prime(tangent_data_df$x)
tangent_data_df$intercept <- tangent_data_df$f_x - tangent_data_df$slope * tangent_data_df$x

# Add legend items to the data
function_data_df$Line_Type <- "Function Line"
tangent_data_df$Line_Type <- "Tangent Line"

# Create a data frame for the zero line
zero_line_df <- data.frame(
  iteration = unique(iterations_df$iteration),
  yintercept = 0,
  Line_Type = "Zero Line"
)

# Combine all data for the legend
legend_data <- rbind(
  data.frame(Line_Type = "Function Line"),
  data.frame(Line_Type = "Tangent Line"),
  data.frame(Line_Type = "Zero Line")
)

# Creating the visualization
viz <- list(
  newtonRaphsonPlot = ggplot() +
    geom_hline(
      aes(yintercept = yintercept, linetype = Line_Type),
      data = zero_line_df, color = "gray", size = 1.5,
      #   showSelected = "iteration",
      help = "This line represents f(x) = 0, showing where we seek the root."
    ) +
    geom_line(
      aes(x = x, y = y, linetype = Line_Type),
      data = function_data_df, color = "black", size = 1.2,
      #   showSelected = "iteration",
      help = "The black curve represents the function f(x), whose root we are approximating."
    ) +
    geom_abline(
      aes(slope = slope, intercept = intercept, linetype = Line_Type),
      data = tangent_data_df, color = "red", size = 1.5,
      showSelected = "iteration",
      help = "The red dashed lines are the tangent lines at each iteration of the Newton-Raphson method."
    ) +
    geom_point(
      aes(x = x, y = f_x),
      data = iterations_df, color = "blue", size = 3, showSelected = "iteration",
      help = "The blue points indicate the successive approximations of the root."
    ) +
    geom_vline(
      aes(xintercept = tangent_intercept),
      data = iterations_df, color = "orange", size = 1, showSelected = "iteration",
      help = "The orange vertical lines mark the next x-values computed at each iteration."
    ) +
    geom_text(
      aes(
        x = tangent_intercept, y = 0,
        label = paste("x:", sprintf("%.4f", tangent_intercept))
      ),
      data = iterations_df, vjust = 0, hjust = 0.5, color = "black", size = 15,
      showSelected = "iteration",
      help = "The text annotations show the numerical value of x at each iteration."
    ) +
    scale_linetype_manual(
      name = "Legend",
      values = c("Zero Line" = "dashed", "Function Line" = "solid", "Tangent Line" = "dotted"),
      breaks = c("Zero Line", "Function Line", "Tangent Line")
    ) +
    scale_x_continuous(
      limits = c(floor(min(x_vals)), ceiling(max(x_vals))),
      breaks = pretty(x_vals, n = 10),
      expand = c(0.05, 0.05)
    ) +
    scale_y_continuous(
      limits = c(floor(min(function_data_df$y)), ceiling(max(function_data_df$y))),
      breaks = pretty(function_data_df$y, n = 10),
      expand = c(0.05, 0.05)
    ) +
    labs(
      title = "Newton-Raphson Method - Root Finding",
      subtitle = "Solving f(x) = 5x³ - 7x² - 40x + 100",
      x = "x",
      y = "f(x)"
    ),
  time = list(variable = "iteration", ms = 1000)
)

# Render the visualization
info <- animint2HTML(viz)

# Test the initial HTML
test_that("visualization of function is correct", {
  expect_true(nrow(function_data_df) > 0, info = "Function data should not be empty.")
  expect_true(all(!is.na(function_data_df$y)), info = "Function values should not contain NA.")
})

test_that("HTML content generated contains correct plot components", {
  expect_true(grepl("Newton-Raphson Method - Root Finding", saveXML(info$html)), 
              info = "Plot title not found in the HTML content.")
})

test_that("Data correctness in iterations_df", {
  expect_true(nrow(iterations_df) > 0, info = "No iterations were performed.")
  expect_true(all(!is.na(iterations_df$f_x)), info = "f(x) values should not contain NA.")
})

test_that("Animation control buttons are there", {
  expect_true(grepl("Show animation controls", saveXML(info$html)), 
              info = "Animation control buttons not found in the HTML content.")
})

# Test the initial state of the legend items
test_that("legend items are initially visible", {
  zero_line <- getNodeSet(info$html, '//g[@class="geom1_hline_newtonRaphsonPlot"]//line')
  function_line <- getNodeSet(info$html, '//g[@class="geom2_line_newtonRaphsonPlot"]//path')
  tangent_line <- getNodeSet(info$html, '//g[@class="geom3_abline_newtonRaphsonPlot"]//line')
  
  expect_true(length(zero_line) > 0, info = "Zero line should be visible initially.")
  expect_true(length(function_line) > 0, info = "Function line should be visible initially.")
  expect_true(length(tangent_line) > 0, info = "Tangent lines should be visible initially.")
})

# Simulate a mouse click on the "Zero Line" legend item
clickID("plot_newtonRaphsonPlot_Line_Type_variable_Zero_Line")
Sys.sleep(0.5)

# updated HTML after the click
after_zero_line_click <- getHTML()

# Testing the updated state after clicking "Zero Line"
test_that("Zero line disappears after click", {
  zero_line <- getNodeSet(after_zero_line_click, '//g[@class="geom1_hline_newtonRaphsonPlot"]//line')
  expect_equal(length(zero_line), 0, info = "Zero line should disappear after click.")
})

# Simulating a mouse click on the "Function Line" legend item
clickID("plot_newtonRaphsonPlot_Line_Type_variable_Function_Line")
Sys.sleep(0.5)

# updated HTML after the click
after_function_line_click <- getHTML()

# Testing the updated state after clicking "Function Line"
test_that("Function line disappears after click", {
  function_line <- getNodeSet(after_function_line_click, '//g[@class="geom2_line_newtonRaphsonPlot"]//path')
  expect_equal(length(function_line), 0, info = "Function line should disappear after click.")
})

# Simulating a mouse click on the "Tangent Line" legend item
clickID("plot_newtonRaphsonPlot_Line_Type_variable_Tangent_Line")
Sys.sleep(0.5)

# Getting the updated HTML after the click
after_tangent_line_click <- getHTML()

# Testing the updated state after clicking "Tangent Line"
test_that("Tangent lines disappear after click", {
  tangent_line <- getNodeSet(after_tangent_line_click, '//g[@class="geom3_abline_newtonRaphsonPlot"]//line')
  expect_equal(length(tangent_line), 0, info = "Tangent lines should disappear after click.")
})
