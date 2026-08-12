acontext("stat bin showSelected")

make <- function(count, stack, facet){
  data.frame(count=count, row=seq_along(count), stack=stack, facet=facet)
}

df <- rbind(
  make(c(1, 1, 1, 2), 1, 1),
  make(c(3, 3, 4, 5), 2, 1),
  make(c(1, 2, 2, 3), 1, 2),
  make(c(3, 4, 4, 4), 2, 2)
)

rect_xpath <- function(panel=1){
  sprintf('//g[@class="PANEL%d"]//g[contains(@class,"geom1_bar_plot")]//rect', panel)
}

get_rect_heights <- function(html, panel=1){
  as.numeric(getPropertyValue(html, rect_xpath(panel), "height"))
}

max_height_by_fill <- function(html, panel=1){
  fills <- getStyleValue(html, rect_xpath(panel), "fill")
  heights <- get_rect_heights(html, panel)
  stats::tapply(heights, fills, max)
}

get_rect_x_width <- function(html, panel=1){
  list(
    x=as.numeric(getPropertyValue(html, rect_xpath(panel), "x")),
    width=as.numeric(getPropertyValue(html, rect_xpath(panel), "width"))
  )
}

select_facet <- function(facet){
  runtime_evaluate(script='document.getElementsByClassName("facet_variable_selector_widget")[0].getElementsByClassName("selectize-input")[0].dispatchEvent(new CustomEvent("click"));')
  sendKey("Backspace")
  remDr$Input$insertText(text=as.character(facet))
  sendKey("Enter")
  Sys.sleep(0.5)
}

stat_bin_showSelected_viz <- function(){
  list(
    plot=ggplot() +
      theme_bw() +
      theme(panel.margin=grid::unit(0, "lines")) +
      geom_histogram(
        aes(count, group=stack, fill=stack),
        showSelected="facet",
        binwidth=1,
        data=df,
        stat="bin",
        position="identity"
      ) +
      facet_grid(facet~.),
    first=list(facet=1),
    selector.types=list(facet="single")
  )
}

test_that("stat_bin with showSelected recalculates after selection", {
  info <- NULL
  expect_no_warning({
    info <<- animint2HTML(stat_bin_showSelected_viz())
  })
  initial_heights <- max_height_by_fill(info$html, 1)
  expect_length(initial_heights, 2)
  expect_gt(length(get_rect_heights(info$html, 1)), 0)
  initial_bounds <- get_rect_x_width(info$html, 1)
  select_facet(2)
  html_after <- getHTML()
  selected_heights <- max_height_by_fill(html_after, 1)
  expect_length(selected_heights, 2)
  expect_gt(length(get_rect_heights(html_after, 1)), 0)
  expect_identical(order(initial_heights), rev(order(selected_heights)))
  selected_bounds <- get_rect_x_width(html_after, 1)
  expect_equal(sort(initial_bounds$x), sort(selected_bounds$x))
  expect_equal(sort(initial_bounds$width), sort(selected_bounds$width))
})
