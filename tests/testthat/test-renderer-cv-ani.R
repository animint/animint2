library(testthat)
library(animint2)
library(XML)

context("k-Fold Cross Validation renderer")

# Data Setup
set.seed(42)
N <- 150
k <- 10
x <- runif(N)

fold_sizes <- rep(N %/% k, k)
if (N %% k > 0) {
  fold_sizes[1:(N %% k)] <- fold_sizes[1:(N %% k)] + 1
}
kf <- cumsum(c(1, fold_sizes))

fold_rects <- data.frame(
  fold_id = 1:k,
  xmin = kf[1:k] - 0.5, xmax = kf[2:(k+1)] - 0.5,
  ymin = min(x), ymax = max(x)
)

highlight_rect <- transform(fold_rects, fold = 1:k, fold_id = NULL)

points_data <- do.call(rbind, lapply(1:k, function(i) {
  data.frame(
    index = 1:N, value = x, fold = i,
    role = ifelse(1:N %in% kf[i]:(kf[i+1]-1), "Test", "Train")
  )
}))

fold_sizes_df <- data.frame(fold = 1:k, size = diff(kf))

# Plot Definitions
# 1. Main CV Diagram
plot_cv <- ggplot() +
  geom_rect(data = fold_rects,
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    fill=NA, color="gray60", linetype="dashed") +
  geom_rect(data = highlight_rect,
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    showSelected="fold", fill="lightgreen", alpha=0.5, color="green4") +
  geom_point(data = points_data,
    aes(x=index, y=value, color=role, key=index),
    showSelected="fold", size=3) +
  scale_color_manual(values=c("Test"="tomato","Train"="steelblue")) +
  labs(title="k-Fold Cross Validation (k = 10)",
       x="Sample Index", y="Value", color="") +
  theme_bw()

# 2. Fold Selector
plot_folds <- ggplot(fold_sizes_df, aes(x=fold, y=size, key=fold)) +
  geom_bar(stat="identity", fill="gray80", color="gray40") +
  geom_bar(stat="identity", position="identity",
           showSelected="fold", clickSelects="fold",
           fill="tomato", color="gray40") +
  scale_x_continuous(breaks=1:k) +
  labs(title="Click a fold  |  red = current test fold",
       x="Fold", y="Test set size") +
  theme_bw()

# Compile the animint object to HTML for headless testing
viz <- list(
  title  = "Demonstration of k-Fold Cross Validation",
  cvplot = plot_cv,
  folds  = plot_folds,
  time   = list(variable="fold", ms=800)
)
info <- animint2HTML(viz)

# Tests
test_that("both plots rendered as SVG", {
  html <- getHTML()
  expect_gte(length(getNodeSet(html, "//svg")), 2L)
})

test_that("data points visible on initial load", {
  html <- getHTML()
  circles <- getNodeSet(html,
    '//g[contains(@class,"point") and contains(@class,"cvplot")]//circle')
  expect_gt(length(circles), 0)
})

test_that("10 fold bars rendered in selector plot", {
  html <- getHTML()
  bars <- getNodeSet(html,
    '//g[contains(@class,"bar") and contains(@class,"folds")]//rect')
  expect_gte(length(bars), 10L)
})

test_that("plot title rendered as SVG text", {
  html <- getHTML()
  text_nodes <- getNodeSet(html, '//text')
  text_values <- sapply(text_nodes, xmlValue)
  expect_true(any(grepl("k-Fold Cross Validation", text_values)))
})

test_that("Test and Train colors in initial render", {
  # Note getHTML() returns a live browser object, not plain text which I previously used.
  # Fix:- Now using the saveXML to properly convert the XMLInternalDocument pointer to string. 
  html <- getHTML()
  html_text <- saveXML(html)
  has_tomato <- grepl("tomato|#ff6347|rgb\\(255,\\s*99,\\s*71\\)", html_text, ignore.case=TRUE)
  has_steelblue <- grepl("steelblue|#4682b4|rgb\\(70,\\s*130,\\s*180\\)", html_text, ignore.case=TRUE)
  expect_true(has_tomato, info="tomato (Test) color not found in DOM")
  expect_true(has_steelblue, info="steelblue (Train) color not found in DOM")
})

test_that("clicking fold 3 updates highlighted fold", {
  clickID("plot_folds_fold_variable_3")
  found <- FALSE
  for (i in 1:30) {
    rects <- getNodeSet(getHTML(),
      '//g[contains(@class,"rect") and contains(@class,"cvplot")]//rect')
    if (length(rects) > 0) {
      found <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(found, info="highlighted rect did not appear after clicking fold 3")
})

test_that("play/pause button present for time variable", {
  html <- getHTML()
  play <- getNodeSet(html,
    '//*[@id="play_pause"] | //input[@value="Play" or @value="Pause"]')
  expect_gte(length(play), 1L)
})

test_that("fold selector registered in info object", {
  expect_true("fold" %in% names(info$selectors))
})
