test_that("Simple rowspan layout works", {
  plot_data <- data.frame(x = 1:3, y = c(2, 4, 6))
  plot_collection <- list(
    LeftPlot = ggplot(plot_data, aes(x, y)) +
      geom_point() +
      ggtitle("left_plot") +
      theme_animint(rowspan = 2),
    TopRightPlot = ggplot(plot_data, aes(x, y)) +
      geom_point() +
      ggtitle("top_right_plot") +
      theme_animint(last_in_row = TRUE),
    BottomRightPlot = ggplot(plot_data, aes(x, y)) +
      geom_point() +
      ggtitle("bottom_right_plot")
  )
  html <- animint2HTML(plot_collection)$html
  # 3 plot tables should be rendered
  tables <- getNodeSet(html, "//table[@style='display: inline-block;']")
  expect_equal(length(tables), 3)
  # Check that at least one cell has rowspan=2
  rowspan_cells <- getNodeSet(html, "//td[@rowspan='2']")
  expect_true(length(rowspan_cells) >= 1)
  # Check that plot titles appear in HTML
  html_text <- saveXML(html)
  expect_true(grepl("left_plot", html_text))
  expect_true(grepl("top_right_plot", html_text))
  expect_true(grepl("bottom_right_plot", html_text))
})