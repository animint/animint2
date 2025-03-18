library(testthat)
library(animint2)
library(XML)

#Newton Raphson Method of root finding
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

# Creating the visualization
viz <- list(
  newtonRaphsonPlot = ggplot() +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed",
               help = "This line represents f(x) = 0, showing where we seek the root.") +
    geom_line(
      data = function_data_df, aes(x = x, y = y),
      color = "black", size = 1.2,
      help = "The black curve represents the function f(x), whose root we are approximating."
    ) +
    geom_abline(
      data = tangent_data_df, aes(slope = slope, intercept = intercept),
      color = "red", linetype = "dashed", alpha = 0.5,
      showSelected = "iteration",
      help = "The red dashed lines are the tangent lines at each iteration of the Newton-Raphson method."
    ) +
    geom_point(
      data = iterations_df, aes(x = x, y = f_x),
      color = "blue", size = 3, showSelected = "iteration",
      help = "The blue points indicate the successive approximations of the root."
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = tangent_intercept),
      color = "orange", size = 1, showSelected = "iteration",
      help = "The orange vertical lines mark the next x-values computed at each iteration."
    ) +
    geom_text(
      data = iterations_df, aes(
        x = tangent_intercept, y = 0,
        label = paste("x:", sprintf("%.4f", tangent_intercept))
      ),
      vjust = 0, hjust = 0.5, color = "black", size = 15,
      showSelected = "iteration",
      help = "The text annotations show the numerical value of x at each iteration."
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

map <- animint2HTML(viz)
html_content <- map$html

# Defining the tests

test_that("visualization of function is correct", {
  expect_true(nrow(function_data_df) > 0, info = "Function data should not be empty.")
  expect_true(all(!is.na(function_data_df$y)), info = "Function values should not contain NA.")
})

test_that("HTML content generated contains correct plot components", {
  expect_true(grepl("Newton-Raphson Method - Root Finding", saveXML(html_content)), 
              info = "Plot title not found in the HTML content.")
})

test_that("Data correctness in iterations_df", {
  expect_true(nrow(iterations_df) > 0, info = "No iterations were performed.")
  expect_true(all(!is.na(iterations_df$f_x)), info = "f(x) values should not contain NA.")
})

test_that("Animation control buttons are there", {
  expect_true(grepl("Show animation controls", saveXML(html_content)), 
              info = "Animation control buttons not found in the HTML content.")
})