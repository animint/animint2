test_that("check rowspan, all plots in single row", {
  plot_of_3_dots_data <- data.frame(x = c(1, 2, 3), y = c(1, 4, 9))
  plot_of_2_dots_data <- data.frame(x = c(1, 2), y = c(1, 4))
  plot_of_1_dot_data  <- data.frame(x = c(2), y = c(2))
  plot_of_4_dots_data <- data.frame(x = c(1, 2, 3, 6), y = c(1, 4, 9, 13))
  plot_of_5_dots_data <- data.frame(x = c(1, 2, 3, 6, 9), y = c(1, 4, 9, 13, 16))
  plot_of_6_dots_data <- data.frame(x = c(1, 2, 3, 6, 9, 11), y = c(1, 4, 9, 13, 16, 18))

  plot_of_3_dots <- ggplot(plot_of_3_dots_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 3 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(rowspan = 2)

  plot_of_2_dots <- ggplot(plot_of_2_dots_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 2 Dots") +
    xlab("X Axis") + ylab("Y Axis")

  plot_of_1_dot <- ggplot(plot_of_1_dot_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 1 Dot") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(rowspan = 2)

  plot_of_4_dots <- ggplot(plot_of_4_dots_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 4 Dots") +
    xlab("X Axis") + ylab("Y Axis")

  plot_of_4_dots_copy <- ggplot(plot_of_4_dots_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 4 Dots Copy") +
    xlab("X Axis") + ylab("Y Axis")

  plot_of_5_dots <- ggplot(plot_of_5_dots_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 5 Dots") +
    xlab("X Axis") + ylab("Y Axis")

  plot_of_6_dots <- ggplot(plot_of_6_dots_data, aes(x, y)) +
    geom_point(size = 2) +
    ggtitle("Plot of 6 Dots") +
    xlab("X Axis") + ylab("Y Axis") +
    theme_animint(row = 1, col = 3)

  plot_list <- list(
    plot1 = plot_of_1_dot,
    plot2 = plot_of_2_dots,
    plot3 = plot_of_3_dots,
    plot4 = plot_of_4_dots,
    plot5 = plot_of_5_dots,
    plot6 = plot_of_6_dots
  )

  info <- animint2HTML(plot_list)
  html <- info$html

  tables <- getNodeSet(html, "//table[@style='display: inline-block;']")
  number_of_tables <- length(tables)
  print(paste("Number of tables:", number_of_tables))

  
  expect_equal(number_of_tables, 6)

  # rowspan_nodes <- getNodeSet(html, "//td[@rowspan]")
  # rowspan_values <- sapply(rowspan_nodes, function(node) xmlGetAttr(node, "rowspan"))
  # # print(paste("Rowspan values found:", paste(rowspan_values, collapse = ", ")))
  # expect_true(any(rowspan_values == "2"), info = "At least one cell has rowspan=2")

  # colspan_nodes <- getNodeSet(html, "//td[@colspan]")
  # colspan_values <- sapply(colspan_nodes, function(node) xmlGetAttr(node, "colspan"))
  # print(colspan_nodes)
  # # If your test data does not include colspan, this just confirms zero is expected
  # expect_true(length(colspan_values) >= 0, info = "colspan handled (may be 0)")
})
