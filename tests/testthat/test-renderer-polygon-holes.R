acontext("Polygon holes via subgroup aesthetic")
library(data.table)

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

test_that("polygon tooltip only shown for point with num=1", {
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
