acontext("Polygon holes via subgroup aesthetic")
library(data.table)

m <- matrix(c(
  0,0,0,0,0,0,
  0,1,1,1,1,0,
  0,1,0,0,1,0,
  0,1,0,0,1,0,
  0,1,1,1,1,0,
  0,0,0,0,0,0
), 6, 6, byrow=TRUE)
clines <- contourLines(
  1:ncol(m), 1:nrow(m),
  m, levels=0.5)
simple.polygon.dt <- data.table(subgroup=seq_along(clines))[
, clines[[subgroup]][c("x","y")]
, by=subgroup][, group := 1][]
simple.point.dt <- data.table(
  row=as.integer(row(m)),
  col=as.integer(col(m)),
  value=as.numeric(m)
)[, id := sprintf("point%d_%d", col, row)][]
viz.simple <- animint(
  selector.types=list(group="multiple"),
  poly=ggplot()+
    geom_point(aes(
      col, row, fill=value, id=id),
      size=5,
      data=simple.point.dt)+
    geom_text(aes(
      col, row, label=id),
      data=simple.point.dt)+
    geom_polygon(
      aes(x, y, group=group, subgroup=subgroup),
      data=simple.polygon.dt,
      alpha=0.5,
      clickSelects="group",
      showSelected="group",
      fill="red"))
info <- animint2HTML(viz.simple)

tooltipID <- function(id){
  selector <- paste0("#", id)
  mouseMoved(selector)
  html <- getHTML()
  tooltip.xpath <- '//div[@class="animint-tooltip"]'
  tooltip_div <- getNodeSet(html, tooltip.xpath)
  list(
    opacity=as.numeric(getStyleValue(html, tooltip.xpath, "opacity")),
    value=xmlValue(tooltip_div))
}

test_that("point3_3 is in hole (no polygon tooltip)", {
  computed <- tooltipID("point3_3")
  expect_identical(computed$opacity, 0)
})

test_that("point3_2 is in polygon (yes polygon tooltip)", {
  computed <- tooltipID("point3_2")
  expect_identical(computed$opacity, 0.7)
  expect_identical(computed$value, "group 1")
})

test_that("point3_1 is in background (no polygon tooltip)", {
  computed <- tooltipID("point3_1")
  expect_identical(computed$opacity, 0)
})
