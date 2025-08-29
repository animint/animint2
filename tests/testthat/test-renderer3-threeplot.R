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
  info <- animint2HTML(plot_collection)
  plot_content <- getNodeSet(info$html, '//td[@class="plot_content"]')
  expect_equal(length(plot_content), 1)
  content_children <- xmlChildren(plot_content[[1]])
  expect_equal(length(content_children), 1) #only table
  # 3 plot tables should be rendered
  tables <- getNodeSet(info$html, "//table[@style='display: inline-block;']")
  expect_equal(length(tables), 3)
  # Check that one cell has rowspan=2
  rowspan_cells <- getNodeSet(info$html, "//td[@rowspan='2']")
  expect_equal(length(rowspan_cells), 1)
  # Check that plot titles appear in HTML
  title_elements <- getNodeSet(info$html, "//text[@class='plottitle']")
  title_vec <- sapply(title_elements, xmlValue)
  expect_identical(title_vec, c("left_plot", "top_right_plot", "bottom_right_plot"))
})
