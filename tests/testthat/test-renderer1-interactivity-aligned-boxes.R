acontext("geom-aligned-boxes")

library(animint2)
data(WorldBank, package = "animint2")

# subset of countries
tracked_countries <- c("United States", "Vietnam", "India", "China", "Brazil",
                      "Nigeria", "Germany", "South Africa")

# Filter WorldBank data
wb <- WorldBank %>%
  filter(country %in% tracked_countries) %>%
  mutate(
    year = as.integer(year),
    group = country
  )

# Label data for aligned boxes
label_data <- wb %>%
  mutate(label = country)

# Text data for year display
year_text_data <- data.frame(
  year = unique(wb$year),
  label = unique(wb$year)
)

viz <- animint(
  worldbankAnim = ggplot() +
    geom_point(
      data = wb,
      aes(x = fertility.rate, y = life.expectancy, color = group, key = country),
      size = 8,
      showSelected = "year",
      clickSelects = "country"
    ) +
    geom_aligned_boxes(
      data = label_data,
      aes(x = fertility.rate, y = life.expectancy, label = label, fill = group, key = country),
      alignment = "vertical", color = "#ffffd1", label_r = "9",
      showSelected = "year",
      clickSelects = "country"
    ) +
    make_text(year_text_data, x = 1, y = 82, label = "label") +
    ggtitle("Life Expectancy vs Fertility Rate") +
    xlab("Fertility Rate") +
    ylab("Life Expectancy"),

  timeSeries = ggplot() +
    geom_line(
      data = wb,
      aes(x = year, y = life.expectancy, group = country, color = group),
      size = 1.5,
      showSelected = "country"
    ) +
    geom_point(
      data = wb,
      aes(x = year, y = life.expectancy, color = group),
      showSelected = "country",
      size = 2
    ) +
    ggtitle("Life Expectancy Over Time (Selected Country)") +
    xlab("Year") +
    ylab("Life Expectancy"),

  time = list(variable = "year", ms = 1000),
  duration = list(year = 800),
  first = list(year = min(wb$year)),
  selector.types = list(country = "multiple")
)

info <- animint2HTML(viz)

# Basic rendering tests
test_that("correct number of aligned box geoms are created", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_worldbankAnim"]//g[@class="geom"]')
  expect_equal(length(box_groups), length(tracked_countries))
})

test_that("each geom has both rect and text elements", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_worldbankAnim"]//g[@class="geom"]')
  for (group in box_groups) {
    rect <- getNodeSet(group, './/rect')
    expect_equal(length(rect), 1)
    text <- getNodeSet(group, './/text')
    expect_equal(length(text), 1)
  }
})

test_that("label text content is correct", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_worldbankAnim"]//g[@class="geom"]')
  actual_texts <- sapply(box_groups, function(group) {
    text_node <- getNodeSet(group, './/text')[[1]]
    xmlValue(text_node)
  })
  expect_true(all(actual_texts %in% tracked_countries))
})

# Collision avoidance tests
test_that("boxes do not overlap initially", {
  check_aligned_box_collisions(
    info$html,
    '//g[@class="geom2_alignedboxes_worldbankAnim"]//g[@class="geom"]'
  )
})

# Interaction tests
test_that("Aligned boxes respond to deselecting and reselecting without disappearing or duplicating", {
  # Helper to extract label texts from aligned boxes
  extract_labels <- function(html_doc) {
    text_nodes <- getNodeSet(html_doc, '//g[@class="geom2_alignedboxes_worldbankAnim"]//g[@class="geom"]/text')
    sapply(text_nodes, xmlValue)
  }

  # Deselect China
  clickID("plot_worldbankAnim_group_variable_China")
  Sys.sleep(0.5)
  info$html_updated1 <- getHTML()
  labels1 <- extract_labels(info$html_updated1)
  expect_false("China" %in% labels1)
  expect_true("India" %in% labels1)

  # Deselect India
  clickID("plot_worldbankAnim_group_variable_India")
  Sys.sleep(0.5)
  info$html_updated2 <- getHTML()
  labels2 <- extract_labels(info$html_updated2)
  expect_false("China" %in% labels2)
  expect_false("India" %in% labels2)

  # Reselect China
  clickID("plot_worldbankAnim_group_variable_China")
  Sys.sleep(0.5)
  info$html_updated3 <- getHTML()
  labels3 <- extract_labels(info$html_updated3)
  expect_true("China" %in% labels3)
  expect_false("India" %in% labels3)

  # Ensure no duplicate labels in the final view
  expect_equal(length(labels3), length(unique(labels3)), info = "No duplicate labels should exist")
})

test_that("labels do not collide even after interaction and movements", {
  # This test ensures that aligned box labels do not overlap
  # even after:
  # 1. The animation has been playing for some time, causing boxes to move
  #    to new positions
  # 2. User interactions such as selection/deselection of countries have occurred. (during previous test)

  # The animation is paused after some movement, and the updated positions of
  # the aligned boxes are checked to verify that they remain non-overlapping.

  Sys.sleep(1) # Let movements of aligned boxes occur 
  # Pause animation
  clickID("plot_show_hide_animation_controls")
  Sys.sleep(0.5)
  clickID("play_pause")
  # HTML after pause
  info$html_paused <- getHTML()
  check_aligned_box_collisions(
    info$html_paused,
    '//g[@class="geom2_alignedboxes_worldbankAnim"]//g[@class="geom"]'
  )
})

# Testing tsv file contents and horizontal alignment positions
library(dplyr)
data(Orange)
# Add factor columns explicitly
Orange <- Orange %>%
  mutate(TreeFactor = as.factor(Tree))

label_data <- Orange %>%
  group_by(Tree) %>%
  filter(age == max(age)) %>%
  ungroup() %>%
  mutate(label = paste("Tree", Tree), TreeFactor = as.factor(Tree))

viz <- list(
  orangeGrowth = ggplot() +
    geom_line(
      data = Orange,
      aes(x = circumference, y = age, group = Tree, color = TreeFactor),
      size = 1.2
    ) +
    geom_aligned_boxes(
      data = label_data,
      aes(x = circumference, y = age, label = label, fill = TreeFactor),
      alignment = "horizontal",
      color = "white"
    ) +
    ggtitle("Orange Tree Growth with Aligned Labels") +
    xlab("Age (days)") +
    ylab("Circumference (mm)")
)
info <- animint2HTML(viz)

# Path to the chunk1 TSV file
chunk1.tsv <- file.path("animint-htmltest", "geom2_alignedboxes_orangeGrowth_chunk1.tsv")

test_that("chunk1.tsv exists", {
  expect_true(file.exists(chunk1.tsv))
})

chunk1 <- read.table(chunk1.tsv, sep = "\t", header = TRUE,
                     comment.char = "", quote = "")

test_that("chunk1 contains expected columns", {
  expected.cols <- c("fill", "x", "y", "label", "showSelected1", "group")
  expect_identical(sort(names(chunk1)), sort(expected.cols))
})

test_that("chunk1 data matches input label_data", {
  # Check number of rows matches number of unique trees
  expect_equal(nrow(chunk1), nrow(label_data))
  # Check labels match
  expect_setequal(chunk1$label, label_data$label)
  # Check no missing data
  expect_true(all(complete.cases(chunk1)))
})

test_that("chunk1 group column matches TreeFactor as numeric levels", {
  expected.groups <- as.numeric(as.factor(label_data$Tree))
  actual.groups <- chunk1$group
  expect_setequal(actual.groups, expected.groups)
})

test_that("boxes do not overlap", {
  check_aligned_box_collisions(
    info$html,
    '//g[contains(@class, "geom2_alignedboxes_orangeGrowth")]//g[@class="geom"]'
  )
})