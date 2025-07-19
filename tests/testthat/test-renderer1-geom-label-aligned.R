acontext("geom-label-aligned")

library(animint2)
library(data.table)
data(WorldBank, package = "animint2")
WorldBank_dt <- as.data.table(WorldBank)
# subset of countries
tracked_countries <- c(
  "United States", "Vietnam", "India", "China", "Brazil",
  "Nigeria", "Mali", "South Africa", "Canada")
# Filter WorldBank data
wb <- WorldBank_dt[
  country %in% tracked_countries &
    !is.na(life.expectancy) & !is.na(fertility.rate),
  .(country, year = as.integer(year), life.expectancy, fertility.rate)]
# Label data for the time series
label_data_line <- wb[, .SD[year == max(year)], by = country]
# Text data for year display
year_text_data <- data.table(year = unique(wb$year))
wb.viz <- list(
  lifeExpectancyPlot = ggplot() +
    geom_line(aes(
      x = year, y = life.expectancy, group = country,
      color = country, key=country),
      size = 1.2,
      data = wb,
      clickSelects = "country",
      showSelected = "country"
    ) +
    geom_label_aligned(aes(
      x = year, y = life.expectancy, label = country,
      fill = country, key = country),
      data = label_data_line,
      alignment = "vertical",
      hjust = 1,
      min_distance = 3,
      size=10,
      color = "white",
      showSelected = "country",
      clickSelects = "country"
    ) +
    ggtitle("Life Expectancy Over Time") +
    xlab("Year") +
    ylab("Life Expectancy (years)"),
  worldbankAnim = ggplot() +
    geom_point(aes(
      x = fertility.rate, y = life.expectancy, color = country, key = country),
      data = wb,
      size = 8,
      showSelected = "year",
      clickSelects = "country"
    ) +
    geom_label_aligned(aes(
      x = fertility.rate, y = life.expectancy,
      label = country, fill = country, key = country),
      data = wb,
      size=5,
      alignment = "vertical", color = "#ffffd1", label_r = 5,
      showSelected = "year",
      clickSelects = "country"
    ) +
    make_text(year_text_data, x = 4, y = 82, label = "year") +
    ggtitle("Life Expectancy vs Fertility Rate") +
    xlab("Fertility Rate") +
    ylab("Life Expectancy"),
  time = list(variable = "year", ms = 3000),
  duration = list(year = 2000, country=2000),
  first = list(year = min(wb$year)),
  selector.types = list(country = "multiple")
)
info <- animint2HTML(wb.viz)

# Basic rendering tests
test_that("correct number of label_aligned geoms are created", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom4_labelaligned_worldbankAnim"]//g[@class="geom"]')
  expect_equal(length(box_groups), length(tracked_countries))
})

test_that("each geom has both rect and text elements", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom4_labelaligned_worldbankAnim"]//g[@class="geom"]')
  for (group in box_groups) {
    rect <- getNodeSet(group, './/rect')
    expect_equal(length(rect), 1)
    text <- getNodeSet(group, './/text')
    expect_equal(length(text), 1)
  }
})

test_that("label text content is correct", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom4_labelaligned_worldbankAnim"]//g[@class="geom"]')
  actual_texts <- sapply(box_groups, getTextValue)
  expect_true(all(actual_texts %in% tracked_countries))
})

test_that("label size is correct", {
  ts_size <- getPropertyValue(info$html, '//g[@class="geom2_labelaligned_lifeExpectancyPlot"]//text', "font-size")
  expect_equal(ts_size, rep("10px", nrow(label_data_line)))
  scatter_size <- getPropertyValue(info$html, '//g[@class="geom4_labelaligned_worldbankAnim"]//text', "font-size")
  expect_equal(scatter_size, rep("5px", nrow(label_data_line)))
})

# Collision avoidance tests
test_that("label boxes in timeSeries plot do not overlap initially", {
  check_aligned_box_collisions(
    info$html,
    '//g[@class="geom2_labelaligned_lifeExpectancyPlot"]//g[@class="geom"]'
  )
})

getLabelY <- function(country){
  box.groups <- getNodeSet(getHTML(), '//g[@class="geom4_labelaligned_worldbankAnim"]//g[@class="PANEL1"]//g[@class="geom"]')
  for (group in box.groups) {
    text.node <- getNodeSet(group, './/text')[[1]]
    if (xmlValue(text.node) == country) {
      return(as.numeric(xmlAttrs(text.node)[["y"]]))
    }
  }
}

test_that("geom_label_aligned shows smooth transition of y-position", {
  clickID("plot_show_hide_animation_controls")
  Sys.sleep(1)
  clickID("play_pause")
  Sys.sleep(1)
  before.y <- getLabelY("China")
  clickID("play_pause")
  Sys.sleep(3)
  during.y <- getLabelY("China")
  Sys.sleep(2)
  after.y <- getLabelY("China")
  expect_true(during.y != after.y, info = "During position should differ from after (smooth transition)")
})

# Interaction tests
test_that("Aligned labels in timeSeries respond to deselecting and reselecting without disappearing or duplicating", {
  extract_labels_ts <- function(html_doc) {
    text_nodes <- getNodeSet(html_doc, '//g[@class="geom2_labelaligned_lifeExpectancyPlot"]//g[@class="geom"]/text')
    sapply(text_nodes, xmlValue)
  }

  # Deselect Brazil
  clickID("plot_lifeExpectancyPlot_country_variable_Brazil")
  Sys.sleep(0.5)
  info$html_ts_1 <- getHTML()
  labels1 <- extract_labels_ts(info$html_ts_1)
  expect_false("Brazil" %in% labels1)
  expect_true("India" %in% labels1)

  # Deselect India
  clickID("plot_lifeExpectancyPlot_country_variable_India")
  Sys.sleep(0.5)
  info$html_ts_2 <- getHTML()
  labels2 <- extract_labels_ts(info$html_ts_2)
  expect_false("Brazil" %in% labels2)
  expect_false("India" %in% labels2)

  # Reselect Brazil
  clickID("plot_lifeExpectancyPlot_country_variable_Brazil")
  Sys.sleep(0.5)
  info$html_ts_3 <- getHTML()
  labels3 <- extract_labels_ts(info$html_ts_3)
  expect_true("Brazil" %in% labels3)
  expect_false("India" %in% labels3)

  # Ensure no duplicate labels
  expect_equal(length(labels3), length(unique(labels3)), info = "No duplicate labels should exist in timeSeries labels")
})
test_that("Aligned labels in timeSeries do not collide after selection/deselection", {
  # interactions already occurred from previous test
  info$html_ts_latest <- getHTML()
  check_aligned_box_collisions(
    info$html_ts_latest,
    '//g[@class="geom2_labelaligned_lifeExpectancyPlot"]//g[@class="geom"]'
  )
})

test_that("label_r sets correct rx and ry values", {
  rect_node <- getNodeSet(
    info$html,
    '//g[@class="geom4_labelaligned_worldbankAnim"]//g[@class="PANEL1"]//g[@class="geom"]//rect'
  )[[1]]
  attrs <- xmlAttrs(rect_node)
  expect_equal(as.numeric(attrs[["rx"]]), 5)
  expect_equal(as.numeric(attrs[["ry"]]), 5)
})

test_that("labels have at least 3px vertical spacing", {
  rects <- getNodeSet(info$html,
    '//g[@class="geom2_labelaligned_lifeExpectancyPlot"]//rect')
  positions <- lapply(rects, function(r) {
    y <- as.numeric(xmlGetAttr(r, "y"))
    h <- as.numeric(xmlGetAttr(r, "height"))
    list(top = y, bottom = y + h)
  })
  positions <- positions[order(sapply(positions, `[[`, "top"))]
  # Calculate vertical gaps: distance from bottom[i] to top[i+1]
  gaps <- mapply(function(a, b) b$top - a$bottom,
                 positions[-length(positions)], positions[-1])
  expect_true(all(gaps >= 3), info = paste("Min gap found:", min(gaps)))
})

# Testing tsv file contents , alignment positions and shrinking mechanism for labels
library(data.table)
data(Orange)
set.seed(42)
Orange <- as.data.table(Orange)
Orange_list <- lapply(1:6, function(i) {
  group_name <- if(i %% 3 == 1) "Fast" else if(i %% 3 == 2) "Medium" else "Slow"
  age_scalar <- if(group_name == "Fast") 1.2 else if(group_name == "Medium") 1.0 else 0.8
  Orange_copy <- copy(Orange)
  Orange_copy[, `:=`(
    Tree = as.numeric(Tree) + (i-1)*100,
    TreeFactor = as.factor(Tree),
    growth_group = group_name,
    circumference = circumference * (1 + (i %% 3)/5) * runif(.N, 0.95, 1.05),
    age = age * age_scalar
  )]
  Orange_copy
})
Orange <- rbindlist(Orange_list)
label_data <- Orange[, .SD[age == max(age)], by = Tree][
  , label := sprintf("Tree %d (%s)", Tree, growth_group)][
  , TreeFactor := as.factor(Tree)]
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
  selected_labels <- label_data[growth_group %in% c("Fast", "Medium", "Slow")]
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

test_that("label = 0 shows non-zero rect size", {
  df <- data.frame(x = 1, y = 1, label = 0)
  viz <- animint2::animint(
    testPlot = ggplot() +
      geom_label_aligned(data = df, aes(x, y, label = label))
  )
  info <- animint2HTML(viz)
  rect.nodes <- getNodeSet(
    info$html,
    '//g[@class="geom1_labelaligned_testPlot"]//rect'
  )
  expect_equal(length(rect.nodes), 1)
  rect <- rect.nodes[[1]]
  rect.attrs <- xmlAttrs(rect)
  width <- as.numeric(rect.attrs[["width"]])
  height <- as.numeric(rect.attrs[["height"]])
  expect_gt(width, 0)
  expect_gt(height, 0)
})
