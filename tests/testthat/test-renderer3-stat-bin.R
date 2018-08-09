acontext("stat bin")

set.seed(1)
make <- function(count, stack, a_facet){
  data.frame(count, row=1:count, stack, a_facet)
}
df <- rbind(
  make(2, 1, 1),
  make(5, 1, 1),
  make(3, 2, 1),
  make(4, 2, 1),
  make(2, 2, 2),
  make(5, 2, 2),
  make(3, 1, 2),
  make(4, 1, 2)
)

test_that("error for stat=bin and showSelected", {
  gg <- a_plot() +
    a_theme_bw()+
    a_theme(panel.margin=grid::unit(0, "lines"))+
    a_geom_bar(
      a_aes(count, group=stack, fill=stack),
      showSelected="a_facet",
      binwidth=1,
      data = df,
      stat = "bin",
      a_position="identity"
    )
  gg+a_facet_grid(a_facet~.)
  complicated <- list(
    plot = gg
  )
  expect_error({
    animint2HTML(complicated)
  }, "showSelected does not work with a_StatBin, problem: a_geom1_bar_plot")
})

test_that("no warning for stat=bin without showSelected", {
  gg <- a_plot() +
    a_theme_bw()+
    a_theme(panel.margin=grid::unit(0, "lines"))+
    a_geom_bar(
      a_aes(count, group=stack, fill=stack),
      binwidth=1,
      data = df,
      stat = "bin",
      a_position="identity"
    )+
    a_facet_grid(a_facet~.)
  complicated <- list(plot = gg)
  expect_no_warning({
    info <- animint2HTML(complicated)
  })
  for(panel in 1:2){
    xpath <- sprintf('//g[@class="PANEL%d"]//rect', panel)
    style.vec <- getStyleValue(info$html, xpath, "fill")
    fill.counts <- table(style.vec)
    expect_equal(length(fill.counts), 2)
  }
})
