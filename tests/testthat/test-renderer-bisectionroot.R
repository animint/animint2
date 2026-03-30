library(testthat)
library(XML)

f <- function(x) x ** 3 + x ** 2 - 2 * x + 2
errf <- function(x) abs(f(x))
lowerlim <- -3
upperlim <- 0
n_iterations <- 15

history_list <- list()
error_list <- list()

a <- lowerlim
b <- upperlim

# calculation
for (i in 1:n_iterations) {
  c <- (a + b) / 2
  
  history_list[[i]] <- data.frame(
    itr  = i,
    a    = a,
    b    = b,
    c    = c,
    f_a  = f(a),
    f_b  = f(b),
    f_c  = f(c)
  )
  
  error_list[[i]] <- data.frame(
    itr = i,
    err = errf(c)
  )
  
  if (f(a) * f(c) < 0) {
    b <- c
  } else if (f(b) * f(c) < 0) {
    a <- c
  }
}

error_list

df <- do.call(rbind, history_list)
errdf <- do.call(rbind, error_list)

series <- seq(-10, 10, length.out = 500)

plotdf <- data.frame(
  x = series,
  y = f(series)
)

# The actual plotting
plot <- ggplot() +
  labs(title = "Bisection Root Finding Method", x = "x", y = "f(x)") + 
  annotate("text", x = 0.8, y = 4.5,
           label = "f(x) == x^3 + x^2 - 2*x + 2",
           parse = TRUE, size = 14, color = "#4c566a") +
  geom_path(mapping = aes(x = x, y = y), 
             data = plotdf, color = "#3b4252", size = 1.3) + 
  
  # midpoint line (teal)
  geom_vline(mapping = aes(xintercept = c), 
             data = df, linewidth = 0.8, color = "#2aa198", showSelected = 'itr') +
  
  # midpoint label
  geom_text(mapping = aes(x = c, y = 0, label = sprintf("c = %.2f", c)), 
            data = df, fontface = "bold", size = 12,
            color = "#1a6e5a", showSelected = 'itr') + 
  
  # boundary lines (warm coral)
  geom_vline(mapping = aes(xintercept = a), 
             data = df, linetype = "dashed", color = "#d08770", showSelected = 'itr') + 
  geom_vline(mapping = aes(xintercept = b), 
             data = df, linetype = "dashed", color = "#d08770", showSelected = 'itr') +
  
  # limits
  scale_y_continuous(limits = c(-2, 5)) +
  scale_x_continuous(limits = c(-3, 2)) + 
  
  # themes
  theme_linedraw(base_size = 14)

itr_df <- data.frame(itr = 1:n_iterations)

err.plot <- ggplot() + 
  labs(title = "Error Analysis", y = "Error range", x = "Iteration") +
  annotate("text", x = 11.5, y = 3.5,
           label = "err(x) = abs(f(c))",
           parse = TRUE, size = 15.5, color = "#4c566a") +
  geom_path(mapping = aes(x = itr, y = err), data = errdf,
            color = "#5e81ac", size = 1.1) + 
  geom_point(mapping = aes(x = itr, y = err, id = itr), data = errdf,
             color = "#5e81ac", fill = "#88c0d0", size = 3, clickSelects = 'itr') + 

  geom_vline(mapping = aes(xintercept = itr), data = errdf, 
    showSelected = 'itr', alpha = 0.8, color = "#88c0d0") + 

  # vertical lines for the err plot
  geom_vline(mapping = aes(xintercept = itr), data = itr_df, clickSelects = 'itr', alpha = 0.4) + 
  # theming
  theme_linedraw(base_size = 14)

# animint stuff
viz <- animint(
  title = "Bisection Method Animation",
  source = plot,
  errorplot = err.plot,
  #duration = list(itr = 250),
  time = list(variable="itr", ms=1000)
)

info <- animint2HTML(viz)

# data correctness

test_that("bisection iteration data is correct", {
  expect_equal(nrow(df), n_iterations)
  expect_true(all(!is.na(df$f_c)), info = "f(c) values should not contain NA.")
  # a < c < b always
  expect_true(all(df$c >= pmin(df$a, df$b) & df$c <= pmax(df$a, df$b)),
              info = "Midpoint c should always be between a and b.")
})

test_that("error data is correct", {
  expect_equal(nrow(errdf), n_iterations)
  expect_true(all(!is.na(errdf$err)), info = "Error values should not contain NA.")
})

test_that("function plot data is correct", {
  expect_equal(nrow(plotdf), 500)
  expect_true(all(!is.na(plotdf$y)), info = "Function values should not contain NA.")
})

# check html structure
test_that("HTML contains bisection plot title", {
  expect_true(grepl("Bisection Root Finding Method", saveXML(info$html)),
              info = "Bisection plot title not found in the HTML content.")
})

test_that("HTML contains error plot title", {
  expect_true(grepl("Error Analysis", saveXML(info$html)),
              info = "Error plot title not found in the HTML content.")
})

test_that("Animation control buttons are present", {
  expect_true(grepl("Show animation controls", saveXML(info$html)),
              info = "Animation control buttons not found in the HTML content.")
})

# check the initial geoms under the main plot

test_that("source plot geoms are initially present", {
  function_curve <- getNodeSet(info$html, '//g[@class="geom2_path_source"]//path')
  green_vline <- getNodeSet(info$html, '//g[@class="geom3_vline_source"]//line')
  midpoint_text <- getNodeSet(info$html, '//g[@class="geom4_text_source"]//text')
  a_vline <- getNodeSet(info$html, '//g[@class="geom5_vline_source"]//line')
  b_vline <- getNodeSet(info$html, '//g[@class="geom6_vline_source"]//line')

  expect_gt(length(function_curve), 0)
  expect_gt(length(green_vline), 0)
  expect_gt(length(midpoint_text), 0)
  expect_gt(length(a_vline), 0)
  expect_gt(length(b_vline), 0)
})

# same for err plot

test_that("errorplot geoms are initially present", {
  error_curve <- getNodeSet(info$html, '//g[@class="geom8_path_errorplot"]//path')
  error_points <- getNodeSet(info$html, '//g[@class="geom9_point_errorplot"]//circle')
  selected_vline <- getNodeSet(info$html, '//g[@class="geom10_vline_errorplot"]//line')

  expect_gt(length(error_curve), 0)
  expect_gt(length(error_points), 0)
  expect_gt(length(selected_vline), 0)
})

# check itr 5 (click selects)
clickID("5")
Sys.sleep(0.5)

after_click_5 <- getHTML()

test_that("source plot updates after clicking iteration 5", {
  green_vlines <- getNodeSet(after_click_5, '//g[@class="geom3_vline_source"]//line')
  expect_gt(length(green_vlines), 0)

  text_labels <- getNodeSet(after_click_5, '//g[@class="geom4_text_source"]//text')
  expect_gt(length(text_labels), 0)
  label_text <- xmlValue(text_labels[[1]])

  # actual c value
  expected_c <- sprintf("c = %.2f", df$c[5])
  expect_true(grepl(expected_c, label_text),
              info = paste("Text label should show", expected_c))
})

test_that("errorplot updates after clicking iteration 5", {
  lightblue_vline <- getNodeSet(after_click_5, '//g[@class="geom10_vline_errorplot"]//line')
  expect_gt(length(lightblue_vline), 0)
})

# itr 10
clickID("10")
Sys.sleep(0.5)

after_click_10 <- getHTML()

test_that("source plot updates after clicking iteration 10", {
  text_labels <- getNodeSet(after_click_10, '//g[@class="geom4_text_source"]//text')
  expect_gt(length(text_labels), 0)
  label_text <- xmlValue(text_labels[[1]])
  expected_c <- sprintf("c = %.2f", df$c[10])
  expect_true(grepl(expected_c, label_text),
              info = paste("Text label should show", expected_c))
})

test_that("errorplot updates after clicking iteration 10", {
  lightblue_vline <- getNodeSet(after_click_10, '//g[@class="geom10_vline_errorplot"]//line')
  expect_gt(length(lightblue_vline), 0)
})
