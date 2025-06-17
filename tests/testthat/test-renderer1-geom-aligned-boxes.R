acontext("geom_aligned_boxes")

set.seed(42)
label_names <- paste("Label", 1:5)
n_timepoints <- 10

line_data <- do.call(rbind, lapply(label_names, function(label) {
  data.frame(
    Time = 1:n_timepoints,
    Value = cumsum(rnorm(n_timepoints, mean = 0.5, sd = 2)) + runif(1, 20, 30),
    Label = label
  )
}))

# Create overlapping labels at final time point
overlapping_value <- 40
labels_to_overlap <- c("Label 1", "Label 2", "Label 3")
line_data$Value[line_data$Label %in% labels_to_overlap & line_data$Time == n_timepoints] <- overlapping_value

# Create label data for the aligned boxes
label_data <- line_data[line_data$Time == n_timepoints, ]
label_data$label <- label_data$Label

viz <- animint(
  syntheticTrend = ggplot() +
    geom_line(
      data = line_data,
      aes(x = Time, y = Value, color = Label, group = Label),
      size = 1.2
    ) +
    geom_aligned_boxes(
      data = label_data,
      aes(x = Time, y = Value, label = label, fill = Label),
      alignment = "vertical"
    ) +
    ggtitle("Synthetic Trends with Smart Aligned Labels") + 
    xlab("Time") + 
    ylab("Value")
)

info <- animint2HTML(viz)

test_that("correct number of aligned box geoms are created", {
  # One <g class="geom"> per label inside geom2 group
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_syntheticTrend"]//g[@class="geom"]')
  expect_equal(length(box_groups), 5)  # One per label
})

test_that("each geom has both rect and text elements", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_syntheticTrend"]//g[@class="geom"]')
  for (group in box_groups) {
    rect <- getNodeSet(group, './/rect')
    expect_equal(length(rect), 1)
    text <- getNodeSet(group, './/text')
    expect_equal(length(text), 1)
  }
})

test_that("box colors match the fill aesthetic (basic check)", {
  label_names <- paste("Label", 1:5)
  for (label in label_names) {
    fill_color <- getStyleValue(info$html, '//g[@class="geom2_alignedboxes_syntheticTrend"]//g[@class="geom"]/rect', "fill")
    expect_match(fill_color, "^rgb\\(", info = paste("Expected a valid RGB fill for", label))
  }
})

test_that("label text content is correct", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_syntheticTrend"]//g[@class="geom"]')
  actual_texts <- sapply(box_groups, function(group) {
    text_node <- getNodeSet(group, './/text')[[1]]
    xmlValue(text_node)
  })
  expected_texts <- paste("Label", 1:5)
  expect_equal(sort(actual_texts), sort(expected_texts))
})

test_that("boxes do not overlap", {
  box_groups <- getNodeSet(info$html, '//g[@class="geom2_alignedboxes_syntheticTrend"]//g[@class="geom"]')
  box_info <- lapply(box_groups, function(group) {
    rect <- getNodeSet(group, './/rect')[[1]]
    attrs <- xmlAttrs(rect)
    list(
      x = as.numeric(attrs["x"]),
      y = as.numeric(attrs["y"]),
      width = as.numeric(attrs["width"]),
      height = as.numeric(attrs["height"])
    )
  })
  for (i in 1:(length(box_info)-1)) {
    for (j in (i+1):length(box_info)) {
      box1 <- box_info[[i]]
      box2 <- box_info[[j]]
      x_overlap <- box1$x < (box2$x + box2$width) && (box1$x + box1$width) > box2$x
      y_overlap <- box1$y < (box2$y + box2$height) && (box1$y + box1$height) > box2$y
      expect_false(x_overlap && y_overlap, info = paste("Boxes", i, "and", j, "overlap"))
    }
  }
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
  box_groups <- getNodeSet(info$html, '//g[contains(@class, "geom2_alignedboxes_orangeGrowth")]//g[@class="geom"]')
  box_info <- lapply(box_groups, function(group) {
    rect <- getNodeSet(group, './/rect')[[1]]
    attrs <- xmlAttrs(rect)
    list(
      x = as.numeric(attrs["x"]),
      width = as.numeric(attrs["width"])
    )
  })

  for (i in 1:(length(box_info) - 1)) {
    for (j in (i + 1):length(box_info)) {
      b1 <- box_info[[i]]
      b2 <- box_info[[j]]
      x_overlap <- b1$x < (b2$x + b2$width) && (b1$x + b1$width) > b2$x
      expect_false(x_overlap, info = paste("Boxes", i, "and", j, "overlap"))
    }
  }
})
