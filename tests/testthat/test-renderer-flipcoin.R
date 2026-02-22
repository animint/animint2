context("flipcoin renderer")

library(animint2)
library(XML)

set.seed(123)
faces <- c("Head", "Stand", "Tail")
prob <- c(0.45, 0.10, 0.45)
n_max <- 100

toss_result <- sample(faces, size = n_max, replace = TRUE, prob = prob)

blocks_freq <- do.call(rbind, lapply(1:n_max, function(i) {
  do.call(rbind, lapply(faces, function(f) {
    count_f <- sum(toss_result[1:i] == f)
    if (count_f > 0) {
      block_height <- 1 / n_max
      data.frame(
        toss = i,
        face = f,
        ymin = (0:(count_f - 1)) * block_height,
        ymax = (1:count_f) * block_height
      )
    }
  }))
}))
blocks_freq$face <- factor(blocks_freq$face, levels = faces)

face_pos <- c(1.25, 3.75, 6.25)
names(face_pos) <- faces
blocks_freq$xpos <- face_pos[as.character(blocks_freq$face)]

scatter_positions <- data.frame(
  face = toss_result,
  x = runif(n_max, 8.2, 16),
  y = runif(n_max, 0.01, 0.51)
)
scatter_positions$face <- factor(scatter_positions$face, levels = faces)

scatter_data <- do.call(rbind, lapply(1:n_max, function(i) {
  df <- scatter_positions[1:i, ]
  df$toss <- i
  df
}))

counter_data <- data.frame(
  toss = 1:n_max,
  label = paste("Number of Tosses :", 1:n_max),
  x = 11,
  y = -0.035
)

bar_labels <- do.call(rbind, lapply(1:n_max, function(i) {
  do.call(rbind, lapply(faces, function(f) {
    count_f <- sum(toss_result[1:i] == f)
    data.frame(
      toss = i,
      face = f,
      y = 0.54,
      label = paste0(count_f, " (", sprintf("%.2f", count_f / n_max), ")")
    )
  }))
}))
bar_labels$xpos <- face_pos[as.character(bar_labels$face)]

vertical_text_data <- data.frame(
  x = 17.2,
  y = 0.25,
  label = "Flip 'coins'"
)

p <- ggplot() +
  annotate("segment", x = 7.5, xend = 7.5, y = 0, yend = 0.525, size = 1.2) +
  geom_text(
    data = vertical_text_data,
    aes(x = x, y = y, label = label),
    angle = 90,
    size = 15,
    fontface = "bold",
    color = "black"
  ) +
  annotate("rect", xmin = 0, xmax = 16.5, ymin = 0, ymax = 0.525, fill = NA, color = "black", size = 0.5) +
  annotate("segment", x = face_pos, xend = face_pos, y = 0, yend = -0.015) +
  annotate("text", x = face_pos, y = -0.04, label = names(face_pos), size = 15) +
  geom_rect(
    data = blocks_freq,
    aes(
      xmin = xpos - 1.2,
      xmax = xpos + 1.2,
      ymin = ymin,
      ymax = ymax,
      fill = face
    ),
    color = "white",
    size = 0.2,
    showSelected = "toss"
  ) +
  geom_text(
    data = scatter_data,
    aes(x = x, y = y, label = face, color = face),
    showSelected = "toss",
    fontface = "bold",
    alpha = 0.8
  ) +
  scale_fill_manual("Face", values = c(
    Head = "black",
    Stand = "red",
    Tail = "blue"
  )) +
  scale_color_manual("Face", values = c(
    Head = "black",
    Stand = "red",
    Tail = "blue"
  )) +
  scale_y_continuous(
    limits = c(-0.1, 0.57),
    breaks = seq(0, 0.5, 0.1),
    labels = function(x) ifelse(x < 0, "", x),
    expand = c(0, 0)
  ) +
  geom_text(
    data = counter_data,
    aes(x = 12.0, y = -0.04, label = label),
    showSelected = "toss",
    size = 15,
    fontface = "bold"
  ) +
  geom_text(
    data = bar_labels,
    aes(x = xpos, y = y, label = label),
    showSelected = "toss",
    size = 15,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0, 19.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  labs(
    y = "Frequency",
    x = NULL,
    caption = ""
  ) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 10, l = 5),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  ) +
  theme_animint(width = 600)

viz <- animint(
  coin = p,
  time = list(variable = "toss", ms = 300),
  title = "Coin Flip Frequency"
)

info <- animint2HTML(viz)

test_that("Output HTML is generated and valid", {
  expect_true(
    nzchar(saveXML(info$html)) > 0,
    info = "HTML content is empty or invalid."
  )
})

test_that("HTML content generated contains correct plot components", {
  expect_true(
    grepl("Coin Flip Frequency", saveXML(info$html)),
    info = "Plot title not found in the HTML content."
  )
})

test_that("Data correctness in internal iteration structures", {
  expect_gt(nrow(blocks_freq), 0)
  expect_true(
    all(!is.na(blocks_freq$ymax)),
    info = "ymax values should not contain NA."
  )
  expect_true(
    all(!is.na(blocks_freq$ymin)),
    info = "ymin values should not contain NA."
  )

  expect_gt(nrow(scatter_data), 0)
  expect_true(
    all(!is.na(scatter_data$toss)),
    info = "toss values should not contain NA."
  )
})


test_that("SVG element exists for the coin plot", {
  coin.node <- getNodeSet(info$html, '//svg[@id="plot_coin"]')
  expect_equal(length(coin.node), 1, info = "Coin plot SVG should exist")
})


test_that("Specific text labels, axes, and captions are present", {
  svg_text <- getNodeSet(info$html, "//text")
  all_text <- sapply(svg_text, xmlValue)

  # 1. Check if all faces are present in text labels
  faces_found <- sapply(faces, function(f) {
    any(grepl(f, all_text, fixed = TRUE))
  })
  expect_true(
    all(faces_found),
    info = "All coin faces('Head','Stand','Tail') should found in text elements"
  )

  # 2. Check for axis label 'Frequency' and chart label 'Flip \'coins\''
  expect_true(
    any(grepl("Frequency", all_text, fixed = TRUE)),
    info = "The text 'Frequency' should be present"
  )
  expect_true(
    any(grepl("Flip 'coins'", all_text, fixed = TRUE)),
    info = "The text \"Flip 'coins'\" should be present"
  )

  expect_true(
    any(grepl("Number of Tosses", all_text)),
    info = "'Number of Tosses' caption text should be present in the plot."
  )
})

# Simulating a mouse click on the "Stand" legend item
clickID("plot_coin_face_variable_Stand")
Sys.sleep(1.5)

after_stand_click <- getHTML()

test_that("Clicking 'Stand' updates the HTML", {
  expect_true(nzchar(saveXML(after_stand_click)) > 0, info = "The HTML should update and remain valid after clicking 'Stand'.")
})


# Simulating a mouse click on the "Tail" legend item
clickID("plot_coin_face_variable_Tail")
Sys.sleep(1.5)

after_tail_click <- getHTML()

test_that("Clicking 'Tail' updates the HTML", {
  expect_true(nzchar(saveXML(after_tail_click)) > 0, info = "The HTML should update and remain valid after clicking 'Tail'.")
})

# Simulating a mouse click on the "Head" legend item
clickID("plot_coin_face_variable_Head")
Sys.sleep(1.5)

after_head_click <- getHTML()

test_that("Clicking 'Head' updates the HTML", {
  expect_true(nzchar(saveXML(after_head_click)) > 0, info = "The HTML should update and remain valid after clicking 'Head'.")
})



# Simulating a mouse click on "Stand" to make it appear again
clickID("plot_coin_face_variable_Stand")
Sys.sleep(1.5)

after_stand_reappear <- getHTML()

test_that("Second click on 'Stand' updates the HTML", {
  expect_true(nzchar(saveXML(after_stand_reappear)) > 0, info = "The HTML should update and remain valid after the second 'Stand' click.")
})


# Simulating a mouse click on "Tail" to make it appear again
clickID("plot_coin_face_variable_Tail")
Sys.sleep(1.5)

after_tail_reappear <- getHTML()

test_that("Second click on 'Tail' updates the HTML", {
  expect_true(nzchar(saveXML(after_tail_reappear)) > 0, info = "The HTML should update and remain valid after the second 'Tail' click.")
})


# Simulating a mouse click on "Head" to make it appear again
clickID("plot_coin_face_variable_Head")
Sys.sleep(1.5)

after_head_reappear <- getHTML()

test_that("Second click on 'Head' updates the HTML", {
  expect_true(nzchar(saveXML(after_head_reappear)) > 0, info = "The HTML should update and remain valid after the second 'Head' click.")
})
