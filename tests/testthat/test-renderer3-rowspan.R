test_that("check rowspan", {
  data_three_dots <- data.frame(x = c(1, 2, 3), y = c(1, 4, 9))
  plot_left_large <- ggplot(data_three_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 3 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 0, col = 0, rowspan = 2)

  data_two_dots <- data.frame(x = c(1, 2), y = c(1, 4))
  plot_top_middle <- ggplot(data_two_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 2 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 0, col = 1)

  data_single_dot <- data.frame(x = 2, y = 2)
  plot_top_right_tall <- ggplot(data_single_dot, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 1 Dot") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 0, col = 2, rowspan = 2)

  plot_top_farthest_right <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(title = "Car Weight vs. MPG") +
    theme(
      plot.title = element_text(color = "purple", size = 20, face = "bold"),
      panel.background = element_rect(fill = "lightyellow"),
      panel.grid.major = element_line(color = "gray", size = 0.8),
      axis.title.x = element_text(color = "blue", size = 14),
      axis.text.y = element_text(color = "red", size = 12),
      legend.position = "bottom",
      legend.background = element_rect(fill = "lightblue", color = "black", size = 0.5)
    ) +
    theme_animint(row = 0, col = 3)

  data_four_dots <- data.frame(x = c(1, 2, 3, 6), y = c(1, 4, 9, 13))
  plot_bottom_right <- ggplot(data_four_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 4 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 2, col = 2)

  plot_bottom_far_right <- ggplot(data_four_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 4 Dots Copy") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 2, col = 3)

  data_five_dots <- data.frame(x = c(1, 2, 3, 6, 9), y = c(1, 4, 9, 13, 16))
  plot_middle_middle <- ggplot(data_five_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 5 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 1, col = 1)

  data_six_dots <- data.frame(x = c(1, 2, 3, 6, 9, 11), y = c(1, 4, 9, 13, 16, 18))
  plot_bottom_left <- ggplot(data_six_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 6 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 2, col = 0)

  data_seven_dots <- data.frame(x = c(1, 2, 3, 6, 9, 11, 12), y = c(1, 4, 9, 13, 16, 18, 19))
  plot_bottom_middle <- ggplot(data_seven_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 7 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 2, col = 1)

  data_eight_dots <- data.frame(x = c(1, 2, 3, 6, 9, 11, 12, 13), y = c(1, 4, 9, 13, 16, 18, 19, 21))
  plot_middle_right_tall <- ggplot(data_eight_dots, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 8 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 1, col = 3, rowspan = 2)

  plot_collection <- list(
    left_panel = plot_left_large,
    top_middle = plot_top_middle,
    top_right_tall = plot_top_right_tall,
    top_farthest_right = plot_top_farthest_right,
    bottom_right = plot_bottom_right,
    bottom_far_right = plot_bottom_far_right,
    middle_middle = plot_middle_middle,
    bottom_left = plot_bottom_left,
    bottom_middle = plot_bottom_middle,
    middle_right_tall = plot_middle_right_tall
  )

  html_info <- animint2HTML(plot_collection)
  table_elements <- getNodeSet(html_info$html, "//table[@style='display: inline-block;']")
  expect_equal(length(table_elements), 10)
})
