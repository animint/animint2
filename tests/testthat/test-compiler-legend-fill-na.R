library(testthat)
acontext("legend fill=NA rendering")

test_that("fill=NA is converted to transparent in legend", {
  viz <- animint(
    fillNA=ggplot()+
      ggtitle("fill=NA")+
      geom_point(aes(
        x, x, color=x),
        fill=NA,
        shape=21,
        size=10,
        data=data.frame(x=1))
  )
  info <- animint2dir(viz, open.browser = FALSE)
  plot_json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  legend_entries <- plot_json$plots$fillNA$legend$x$entries
  expect_equal(nrow(legend_entries), 1)
  expect_equal(legend_entries$pointfill[1], "transparent")
})

test_that("fill=NA works the same as fill='transparent'", {
  viz <- animint(
    fillNA=ggplot()+
      ggtitle("fill=NA")+
      geom_point(aes(
        x, x, color=x),
        fill=NA,
        shape=21,
        size=10,
        data=data.frame(x=1)),
    filltransparent=ggplot()+
      ggtitle("fill=transparent")+
      geom_point(aes(
        x, x, color=x),
        fill="transparent",
        shape=21,
        size=10,
        data=data.frame(x=1))
  )
  info <- animint2dir(viz, open.browser = FALSE)
  plot_json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  fillNA_entry <- plot_json$plots$fillNA$legend$x$entries
  filltransparent_entry <- plot_json$plots$filltransparent$legend$x$entries
  expect_equal(fillNA_entry$pointfill[1], "transparent")
  expect_equal(filltransparent_entry$pointfill[1], "transparent")
  expect_equal(fillNA_entry$pointfill[1], filltransparent_entry$pointfill[1])
})

test_that("legend with multiple entries and mixed fill values", {
  test_data <- data.frame(
    x = 1:3,
    y = 1:3,
    group = c("A", "B", "C")
  )
  viz <- animint(
    plot=ggplot(test_data, aes(x, y, color=group))+
      geom_point(shape=21, size=5, fill=NA)
  )
  info <- animint2dir(viz, open.browser = FALSE)
  plot_json <- jsonlite::fromJSON(file.path(info$out.dir, "plot.json"))
  legend_entries <- plot_json$plots$plot$legend$group$entries
  expect_equal(nrow(legend_entries), 3)
  for (i in 1:3) {
    expect_equal(legend_entries$pointfill[i], "transparent",
                 info=paste("Entry", i, "should have transparent fill"))
  }
})
