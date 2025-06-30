acontext("geom-label-aligned")

library(animint2)
library(dplyr)
data(WorldBank, package = "animint2")

# subset of countries
tracked_countries <- c("United States", "Vietnam", "India", "China", "Brazil",
                      "Nigeria", "Germany", "South Africa")

# Filter WorldBank data
wb <- subset(WorldBank, country %in% tracked_countries) %>%
  mutate(
    year = as.integer(year),
    group = country
  )

# Label data for aligned labels
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
    geom_label_aligned(
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
test_that("correct number of label_aligned geoms are created", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_labelaligned_worldbankAnim"]//g[@class="geom"]')
  expect_equal(length(box_groups), length(tracked_countries))
})

test_that("each geom has both rect and text elements", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_labelaligned_worldbankAnim"]//g[@class="geom"]')
  for (group in box_groups) {
    rect <- getNodeSet(group, './/rect')
    expect_equal(length(rect), 1)
    text <- getNodeSet(group, './/text')
    expect_equal(length(text), 1)
  }
})

test_that("label text content is correct", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_labelaligned_worldbankAnim"]//g[@class="geom"]')
  actual_texts <- sapply(box_groups, function(group) {
    text_node <- getNodeSet(group, './/text')[[1]]
    xmlValue(text_node)
  })
  expect_true(all(actual_texts %in% tracked_countries))
})

# Collision avoidance tests
test_that("label boxes do not overlap initially", {
  check_aligned_box_collisions(
    info$html,
    '//g[@class="geom2_labelaligned_worldbankAnim"]//g[@class="geom"]'
  )
})

# Interaction tests
test_that("Aligned Labels respond to deselecting and reselecting without disappearing or duplicating", {
  # Helper to extract label texts from aligned labels
  extract_labels <- function(html_doc) {
    text_nodes <- getNodeSet(html_doc, '//g[@class="geom2_labelaligned_worldbankAnim"]//g[@class="geom"]/text')
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
  # 1. The animation has been playing for some time, causing label boxes to move
  #    to new positions
  # 2. User interactions such as selection/deselection of countries have occurred. (during previous test)

  # The animation is paused after some movement, and the updated positions of
  # the aligned labels are checked to verify that they remain non-overlapping.

  Sys.sleep(1) # Let movements of aligned labels occur 
  # Pause animation
  clickID("plot_show_hide_animation_controls")
  Sys.sleep(0.5)
  clickID("play_pause")
  # HTML after pause
  info$html_paused <- getHTML()
  check_aligned_box_collisions(
    info$html_paused,
    '//g[@class="geom2_labelaligned_worldbankAnim"]//g[@class="geom"]'
  )
})

# Testing tsv file contents , horizontal alignment positions and shrinking mechanism for labels
library(dplyr)
data(Orange)
set.seed(42)
Orange <- bind_rows(
  lapply(1:6, function(i) {
    Orange %>%
      mutate(
        Tree = as.numeric(Tree) + (i-1)*100,  # Trees 101-105, 201-205, etc.
        TreeFactor = as.factor(Tree),
        # growth groups with some natural overlap
        growth_group = case_when(
          i %% 3 == 1 ~ "Fast",
          i %% 3 == 2 ~ "Medium", 
          TRUE ~ "Slow"
        ),
        circumference = circumference * (1 + (i %% 3)/5) * runif(nrow(Orange), 0.95, 1.05),
        age = age * (1 + (i %% 3)/10) * runif(nrow(Orange), 0.98, 1.02)
      )
  })
)
label_data <- Orange %>%
  group_by(Tree) %>%
  filter(age == max(age)) %>%
  ungroup() %>%
  mutate(
    label = sprintf("Tree %d (%s)", Tree, growth_group),
    TreeFactor = as.factor(Tree)
  )
viz <- list(
  orangeGrowth = ggplot() +
    geom_line(
      data = Orange,
      aes(x = circumference, y = age, group = Tree, color = growth_group, id = paste0(growth_group, Tree)),
      size = 1.5,
      clickSelects = "Tree",
      showSelected = "growth_group",
      alpha = 0.7, alpha_off = 0.1
    ) +
    geom_label_aligned(
      data = label_data,
      aes(x = circumference, y = age, label = label, fill = growth_group, id = paste0(growth_group, Tree)),
      alignment = "horizontal",
      color = "white",
      showSelected = "Tree",
      clickSelects = "Tree"
    ) +
    scale_color_manual(
      values = c(Fast = "#E41A1C", Medium = "#377EB8", Slow = "#4DAF4A"),
      name = "Growth Rate"
    ) +
    scale_fill_manual(
      values = c(Fast = "#E41A1C", Medium = "#377EB8", Slow = "#4DAF4A"),
      name = "Growth Rate"
    ) +
    ggtitle("Orange Tree Growth Patterns with Natural Overlap") +
    xlab("Circumference (mm)") +
    ylab("Age (days)") +
    theme_bw(),
  first = list(growth_group = c("Fast","Medium","Slow"), 
              Tree = c(101, 102, 103, 104, 201, 202, 203, 204, 301, 302)),
  selector.types = list(Tree = "multiple"),
  title = "Orange Tree Growth Analysis"
)
info <- animint2HTML(viz)

# Path to the chunk1 TSV file
chunk1.tsv <- file.path("animint-htmltest", "geom2_labelaligned_orangeGrowth_chunk1.tsv")

test_that("chunk1.tsv exists", {
  expect_true(file.exists(chunk1.tsv))
})

chunk1 <- read.table(chunk1.tsv, sep = "\t", header = TRUE,
                     comment.char = "", quote = "")

test_that("chunk1 contains expected columns", {
  expected.cols <- c("fill", "x", "y", "label","id", "showSelected1", "showSelected2", "clickSelects", "group")
  expect_identical(sort(names(chunk1)), sort(expected.cols))
})

test_that("chunk1 data matches label_data for initially selected growth groups", {
  selected_labels <- label_data %>% filter(growth_group %in% c("Fast", "Medium", "Slow"))
  expect_equal(nrow(chunk1), nrow(selected_labels))
  expect_setequal(chunk1$label, selected_labels$label)
  expect_true(all(complete.cases(chunk1)))
})

test_that("initial label boxes do not overlap", {
  check_aligned_box_collisions(
    info$html,
    '//g[contains(@class, "geom2_labelaligned_orangeGrowth")]//g[@class="geom"]'
  )
})

test_that("initial labels are within plot boundaries", {
  plot_xlim <- info$plots$orangeGrowth$layout$panel_ranges[[1]]$x.range
  plot_ylim <- info$plots$orangeGrowth$layout$panel_ranges[[1]]$y.range
  
  expect_true(all(chunk1$x >= plot_xlim[1] & chunk1$x <= plot_xlim[2]))
  expect_true(all(chunk1$y >= plot_ylim[1] & chunk1$y <= plot_ylim[2]))
})

# Simulate clicking on multiple Medium group tree lines that are close together in space.
# These are expected to be positioned at the top of the plot where horizontal alignment
# can lead to overlaps, and label shrinking should occur to accommodate them.
clickID("Medium401")
clickID("Medium402")
clickID("Medium405")
clickID("Medium105")

Sys.sleep(1)

# ─────────────────────────────────────────────────────────────────────────────
# Test: Confirm all labels are within plot boundaries after new selections.
# This validates that when there is not enough room for all of the boxes,
# the Optimisation function shrinks the font size until all labels fit in the available space
test_that("All labels after Medium selections are within plot boundaries", {
  plot_xlim <- info$plots$orangeGrowth$layout$panel_ranges[[1]]$x.range
  plot_ylim <- info$plots$orangeGrowth$layout$panel_ranges[[1]]$y.range
  label_positions <- getNodeSet(
    info$html,
    '//g[contains(@class, "geom2_labelaligned_orangeGrowth")]//g[@class="geom"]//text'
  )
  x_vals <- sapply(label_positions, function(node) as.numeric(xmlAttrs(node)[["x"]]))
  y_vals <- sapply(label_positions, function(node) as.numeric(xmlAttrs(node)[["y"]]))
  expect_true(all(x_vals >= plot_xlim[1] & x_vals <= plot_xlim[2]))
  expect_true(all(y_vals >= plot_ylim[1] & y_vals <= plot_ylim[2]))
})

# ─────────────────────────────────────────────────────────────────────────────
# Test: Ensure that no label boxes are overlapping after the new selections and label shrinking.
# This checks that the QP solver successfully avoids overlaps even after label shrinking and crowding.
test_that("No label overlaps occur after selecting Medium trees", {
  check_aligned_box_collisions(
    info$html,
    '//g[contains(@class, "geom2_labelaligned_orangeGrowth")]//g[@class="geom"]'
  )
})

# ─────────────────────────────────────────────────────────────────────────────
# Test: Verify that the font size of Medium group labels (labels close to each other in this case) has decreased
# after adding more crowded labels, indicating the shrinking mechanism is working correctly.
test_that("Font size shrinks for Medium labels after crowding (vs. initial)", {
  # XPath to target the <text> elements inside <g class="geom" id="MediumXXX">
  medium_label_text_xpath <- '//g[@class="geom2_labelaligned_orangeGrowth"]//g[starts-with(@id, "Medium")]//text'
  initial_text_nodes <- getNodeSet(info$html, medium_label_text_xpath)
  initial_font_sizes_num <- sapply(initial_text_nodes, function(node) {
    as.numeric(gsub("px", "", xmlGetAttr(node, "font-size")))
  })
  # Ensure we found Medium labels
  expect_true(length(initial_font_sizes_num) > 0, 
              info = "No Medium group labels found in initial plot")
  updated_html <- getHTML()
  updated_text_nodes <- getNodeSet(updated_html, medium_label_text_xpath)
  updated_font_sizes_num <- sapply(updated_text_nodes, function(node) {
    as.numeric(gsub("px", "", xmlGetAttr(node, "font-size")))
  })
  expect_true(all(updated_font_sizes_num < initial_font_sizes_num),
              info = paste("Font sizes did not decrease as expected:",
                          "Initial sizes:", paste(initial_font_sizes_num, collapse=", "),
                          "Updated sizes:", paste(updated_font_sizes_num, collapse=", ")))
})