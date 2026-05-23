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
      aes(x, y, group=group, subgroup=subgroup, tooltip=paste("group", group)),
      data=simple.polygon.dt,
      alpha=0.5,
      fill="red"))
info <- animint2HTML(viz.simple)

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

## more complex test with 3 polygons.
m.list <- list(
  island=rbind(
    c(0,0,0,0,0,0,0),
    c(0,1,1,1,1,1,0),
    c(0,1,0,0,0,1,0),
    c(0,1,0,1,0,1,0),
    c(0,1,0,0,0,1,0),
    c(0,1,1,1,1,1,0),
    c(0,0,0,0,0,0,0)),
  hole=rbind(
    c(0,0,0,0,0,0,0),
    c(0,1,1,1,1,1,0),
    c(0,1,0,0,0,1,0),
    c(0,1,0,0,0,1,0),
    c(0,1,0,0,0,1,0),
    c(0,1,1,1,1,1,0),
    c(0,0,0,0,0,0,0)),
  filled=rbind(
    c(0,0,0,0,0,0,0),
    c(0,1,1,1,1,1,0),
    c(0,1,1,1,1,1,0),
    c(0,1,1,1,1,1,0),
    c(0,1,1,1,1,1,0),
    c(0,1,1,1,1,1,0),
    c(0,0,0,0,0,0,0)))
poly.list  <- list()
point.list <- list()
for(grp.i in seq_along(m.list)){
  offset <- grp.i * 7
  group <- names(m.list)[[grp.i]]
  m <- m.list[[grp.i]]
  clines <- contourLines(
    1:ncol(m), 1:nrow(m), m, levels=0.5)
  poly.list[[grp.i]] <- data.table(
    subgroup=seq_along(clines)
  )[, with(clines[[subgroup]], data.table(
    group, x=x+offset, y
  )), by=subgroup]
  point.list[[grp.i]] <- data.table(
    row=as.integer(row(m)),
    col=as.integer(col(m))+offset,
    num=as.numeric(m),
    group
  )[, id := sprintf("%s_%d_%d", group, col, row)][]
}
poly.dt <- do.call(rbind, poly.list)
point.dt <- do.call(rbind, point.list)

viz.full <- animint(
  poly=ggplot()+
    geom_point(aes(
      col, row, fill=num, id=id),
      data=point.dt,
      color="red", size=3)+
    geom_polygon(aes(
      x, y, group=group, subgroup=subgroup, tooltip=group),
      data=poly.dt,
      alpha=0.5,
      fill="steelblue")+
    theme_animint(width=800, height=300))
info.full <- animint2HTML(viz.full)

test_that("island_mid is in polygon (yes polygon tooltip)", {
  computed.dt.list <- list()
  for(point.i in 1:nrow(point.dt)){
    point.row <- point.dt[point.i]
    computed <- tooltipID(point.row$id)
    computed.dt.list[[point.i]] <- data.table(
      point.row, as.data.table(computed))
  }
  computed.dt <- rbindlist(computed.dt.list)[
  , expected := ifelse(num==0, 0, 0.7)
  ][]
  computed.dt[, expect_equal(opacity, expected)]
  computed.dt[num==1, expect_equal(value, group)]
})
