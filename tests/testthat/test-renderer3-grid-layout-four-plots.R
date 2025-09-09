test_that("HTML layout includes rowspan/colspan and FooterRight has none", {
  base_data <- data.frame(x = 1:3, y = c(2, 4, 6))
  plot_list <- list(
    LargeLeftPanel = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Left Side Large") +
      theme_animint(rowspan = 2, width=200, height=400),
    TopRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Top Right") +
      theme_animint(last_in_row = TRUE, width=200, height=200),
    BottomRight = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Bottom Right") +
      theme_animint(last_in_row = TRUE, width=200, height=200),
    Footer = ggplot(base_data, aes(x, y)) +
      geom_point() +
      ggtitle("Footer")+
      theme_animint(colspan=2, width=400, height=200)
  )
  info <- animint2HTML(plot_list)  # render to HTML
  all_svg_plots <- getNodeSet(info$html, "//svg")  # get all SVGs
  expect_equal(length(all_svg_plots), 4)  # expect 4 plots
  td_with_rowspan <- getNodeSet(info$html, "//td[@rowspan]")  # cells with rowspan
  rowspan_values <- sapply(td_with_rowspan, xmlGetAttr, "rowspan")
  expect_equal(sum(rowspan_values == "2"), 1)  # one rowspan=2
  td_with_colspan <- getNodeSet(info$html, "//td[@colspan]")  # cells with colspan
  colspan_values <- sapply(td_with_colspan, xmlGetAttr, "colspan")
  expect_equal(sum(colspan_values == "2"), 1)  # one colspan=2
  footer_svg <- getNodeSet(info$html, "//svg[contains(@id, 'Footer')]")  # find FooterRight plot
  expect_equal(length(footer_svg), 1, info = "Should find exactly one Footer SVG")  # exactly one
  footer_td <- xmlParent(xmlParent(footer_svg[[1]]))  # get td of Footer
  expect_equal(xmlGetAttr(footer_td, "colspan", default = NA_character_), NA_character_)  # no colspan
  expect_equal(xmlGetAttr(footer_td, "rowspan", default = NA_character_), NA_character_)  # no rowspan
})
