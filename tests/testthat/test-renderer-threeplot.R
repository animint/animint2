test_that("Simple rowspan layout works", {
  base_data <- data.frame(x = 1:3, y = c(2, 4, 6))

  plot_list <- list(
    First = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("first") +
      theme_animint(rowspan = 2),

    Second = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("second") +
      theme_animint(last_in_row = TRUE),

    Third = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("third")
  )

  html <- animint2HTML(plot_list)$html

  # 3 plot tables should be rendered
  tables <- getNodeSet(html, "//table[@style='display: inline-block;']")
  expect_equal(length(tables), 3)

  # Check that at least one cell has rowspan=2
  rowspan_cells <- getNodeSet(html, "//td[@rowspan='2']")
  expect_true(length(rowspan_cells) >= 1)

  # Check that plot titles appear in HTML
  html_text <- saveXML(html)
  expect_true(grepl("first", html_text))
  expect_true(grepl("second", html_text))
  expect_true(grepl("third", html_text))
})
