test_that("HTML layout includes rowspan and colspan attributes", {

  base_data <- data.frame(x = 1:3, y = c(2, 4, 6))

  plot_list <- list(
    LargeLeftPanel = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Left Side Large") +
      theme_animint(rowspan = 2, colspan = 1),

    TopRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Top Right") +
      theme_animint(last_in_row = TRUE),  # End first row

    BottomRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Bottom Right") +
      theme_animint(last_in_row = TRUE),  # End second row

    FooterLeft = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer Left"),

    FooterMiddle = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer Middle"),

    FooterRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer Right") +
      theme_animint( last_in_row = TRUE) +
      theme_animint(colspan = 2)
)

  info <- animint2HTML(plot_list)
  html <- info$html

  all_plot_tables <- getNodeSet(html, "//table[@style='display: inline-block;']")
  expect_equal(length(all_plot_tables), 6)


  td_with_rowspan <- getNodeSet(html, "//td[@rowspan]")
  rowspan_values <- sapply(td_with_rowspan, xmlGetAttr, "rowspan")
  expect_true(any(rowspan_values == "2"), info = "Expect one or more cells with rowspan=2")

  td_with_colspan <- getNodeSet(html, "//td[@colspan]")
  colspan_values <- sapply(td_with_colspan, xmlGetAttr, "colspan")
  expect_true(any(colspan_values == "2"), info = "Expect one or more cells with colspan=2")
 
  html_lines <- XML::saveXML(html)
  expect_true(any(grepl("Left Side Large", html_lines)))
  expect_true(any(grepl("Footer Right", html_lines)))
})
