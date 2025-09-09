expect_name <- function(x,N)expect_identical(xmlName(x),N)
expect_svg <- function(td,N){
  expect_name(td, "td")
  node_list <- getNodeSet(bottom_right_td, "//svg")
  svg_node <- node_list[[1]]
  svg_id <- xmlAttrs(svg_node)[["id"]]
  expect_identical(svg_id, paste0("plot_",N))
}
test_that("Simple rowspan layout works", {
  plot_data <- data.frame(x = 1:3, y = c(2, 4, 6))
  plot_collection <- list(
    Left = ggplot(plot_data, aes(x, y)) +
      geom_point() +
      theme_animint(rowspan = 2, height=800) +
      ggtitle("Left")  ,
    TopRight = ggplot(plot_data, aes(x, y)) +
      geom_point() +
      theme_animint(last_in_row = TRUE) +
      ggtitle("TopRight") ,
    BottomRight = ggplot(plot_data, aes(x, y)) +
      geom_point() +
      ggtitle("BottomRight")
  )
  info <- animint2HTML(plot_collection)
  td_plot_content <- getNodeSet(info$html, "//td[@class='plot_content']")
  expect_equal(length(td_plot_content), 1)
  td_children <- xmlChildren(td_plot_content[[1]])
  expect_equal(length(td_children), 1)
  grid_layout_table <- td_children[[1]]
  expect_name(grid_layout_table, "table")
  table_children <- xmlChildren(grid_layout_table)
  expect_equal(length(table_children), 2)
  ## First row should have two tds.
  first_tr <- table_children[[1]]
  expect_name(first_tr, "tr")
  first_td_list <- xmlChildren(first_tr)
  expect_equal(length(first_td_list), 2)
  expect_svg(first_td_list[[1]], "Left")
  expect_svg(first_td_list[[2]], "TopRight")
  ## second row should have one td.
  second_tr <- table_children[[2]]
  expect_name(second_tr, "tr")
  second_td_list <- xmlChildren(second_tr)
  expect_equal(length(second_td_list), 1)
  expect_svg(second_td_list[[1]], "BottomRight")
})
