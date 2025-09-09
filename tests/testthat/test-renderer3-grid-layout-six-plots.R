test_that("HTML layout includes rowspan/colspan and FooterRight has none", {
  base_data <- data.frame(x = 1:3, y = c(2, 4, 6))
  plot_list <- list(
    LargeLeftPanel = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Left Side Large") +
      theme_animint(rowspan = 2, colspan = 1),
    TopRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Top Right") +
      theme_animint(colspan = 2, last_in_row = TRUE),
    BottomRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Bottom Right") +
      theme_animint(last_in_row = TRUE),
    FooterLeft = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer Left"),
    FooterMiddle = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer Middle"),
    FooterRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer Right")
  )
  info <- animint2HTML(plot_list)  # render to HTML
  html <- info$html
  all_svg_plots <- getNodeSet(html, "//svg")  # get all SVGs
  expect_equal(length(all_svg_plots), 6)  # expect 6 plots
  td_with_rowspan <- getNodeSet(html, "//td[@rowspan]")  # cells with rowspan
  rowspan_values <- sapply(td_with_rowspan, xmlGetAttr, "rowspan")
  expect_equal(sum(rowspan_values == "2"), 1)  # one rowspan=2
  td_with_colspan <- getNodeSet(html, "//td[@colspan]")  # cells with colspan
  colspan_values <- sapply(td_with_colspan, xmlGetAttr, "colspan")
  expect_equal(sum(colspan_values == "2"), 1)  # one colspan=2
  footer_right_svg <- getNodeSet(html, "//svg[contains(@id, 'FooterRight')]")  # find FooterRight plot
  expect_equal(length(footer_right_svg), 1, info = "Should find exactly one FooterRight SVG")  # exactly one
  footer_right_td <- xmlParent(xmlParent(footer_right_svg[[1]]))  # get td of FooterRight
  expect_equal(xmlGetAttr(footer_right_td, "colspan", default = NA_character_), NA_character_)  # no colspan
  expect_equal(xmlGetAttr(footer_right_td, "rowspan", default = NA_character_), NA_character_)  # no rowspan
})
