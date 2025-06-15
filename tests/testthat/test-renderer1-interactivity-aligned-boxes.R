library(gapminder)

# subset of countries
tracked_countries <- c("United States", "Vietnam", "India", "China", "Brazil",
  "Nigeria", "Germany", "South Africa")

# Filter gapminder data
gm <- gapminder %>%
  filter(country %in% tracked_countries) %>%
  mutate(
    year = as.integer(year),
    group = country
  )

# Label data for aligned boxes
label_data <- gm %>%
  mutate(label = country)

# Text data for year display
year_text_data <- data.frame(
  year = unique(gm$year),
  label = unique(gm$year)
)

viz <- animint(
  gapminderAnim = ggplot() +
    geom_point(
      data = gm,
      aes(x = gdpPercap, y = lifeExp, color = group, key = country),
      size = 8,
      showSelected = "year",
      clickSelects = "country"
    ) +
    geom_aligned_boxes(
      data = label_data,
      aes(x = gdpPercap, y = lifeExp, label = label, fill = group, key = country),
      alignment = "vertical", color = "#560078",label_r = "9",
      showSelected = "year",
      clickSelects = "country"
    ) +
    scale_x_log10() +
    make_text(year_text_data, x = 1e3, y = 82, label = "label") +
    ggtitle("Life Expectancy vs GDP Per Capita") +
    xlab("GDP Per Capita (log scale)") +
    ylab("Life Expectancy"),

  timeSeries = ggplot() +
    geom_line(
      data = gm,
      aes(x = year, y = lifeExp, group = country, color = group),
      size = 1.5,
      showSelected = "country"
    ) +
    geom_point(
      data = gm,
      aes(x = year, y = lifeExp, color = group),
      showSelected = "country",
      size = 2
    ) +
    ggtitle("Life Expectancy Over Time (Selected Country)") +
    xlab("Year") +
    ylab("Life Expectancy"),

  time = list(variable = "year", ms = 1500),
  duration = list(year = 800),
  first = list(year = 1952),
  selector.types = list(country = "multiple")
)

info <- animint2HTML(viz)

test_that("Aligned boxes respond to deselecting and reselecting without disappearing or duplicating", {
  # Helper to extract label texts from aligned boxes
  extract_labels <- function(html_doc) {
    text_nodes <- getNodeSet(html_doc, '//g[@class="geom2_alignedboxes_gapminderAnim"]//g[@class="geom"]/text')
    sapply(text_nodes, xmlValue)
  }

  # Deselect China
  clickID("plot_gapminderAnim_group_variable_China")
  Sys.sleep(0.5)
  info$html_updated1 <- getHTML()
  labels1 <- extract_labels(info$html_updated1)
  expect_false("China" %in% labels1)
  expect_true("India" %in% labels1)

  # Deselect India
  clickID("plot_gapminderAnim_group_variable_India")
  Sys.sleep(0.5)
  info$html_updated2 <- getHTML()
  labels2 <- extract_labels(info$html_updated2)
  expect_false("China" %in% labels2)
  expect_false("India" %in% labels2)

  # Reselect China
  clickID("plot_gapminderAnim_group_variable_China")
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

  # Pause animation
  clickID("plot_show_hide_animation_controls")
  Sys.sleep(0.5)
  clickID("play_pause")
  # HTML after pause
  info$html_paused <- getHTML()
  # Extracting aligned box positions after pause
  box_groups <- getNodeSet(info$html_paused, '//g[@class="geom2_alignedboxes_gapminderAnim"]//g[@class="geom"]')
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
  # Checking for pairwise overlaps
  for (i in 1:(length(box_info)-1)) {
    for (j in (i+1):length(box_info)) {
      box1 <- box_info[[i]]
      box2 <- box_info[[j]]
      x_overlap <- box1$x < (box2$x + box2$width) && (box1$x + box1$width) > box2$x
      y_overlap <- box1$y < (box2$y + box2$height) && (box1$y + box1$height) > box2$y
      expect_false(
        x_overlap && y_overlap,
        info = paste("Boxes", i, "and", j, "overlap at paused year:", old.year)
      )
    }
  }
})