library(data.table)
panel_group <- function(x, y, panel, g)data.table(x, y, panel, g)

test_that("common chunk has x and colour not PANEL", {
  line_dt <- rbind(
    panel_group(0:1, rnorm(2), "A", "left"),
    panel_group(0:1, rnorm(2), "B", "left"),
    panel_group(0:1, rnorm(2), "C", "left"),
    panel_group(2:3, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_line(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel")+
      facet_grid(. ~ panel))
  info <- animint2HTML(viz)
  common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","PANEL","y"))
})

test_that("common chunk has x and colour", {
  line_dt <- rbind(
    panel_group(0:1, rnorm(2), "A", "left"),
    panel_group(0:1, rnorm(2), "B", "left"),
    panel_group(0:1, rnorm(2), "C", "left"),
    panel_group(2:3, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_line(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2HTML(viz)
  common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","y"))
})

test_that("common chunk has colour", {
  line_dt <- rbind(
    panel_group(0:1, rnorm(2), "A", "left"),
    panel_group(1:2, rnorm(2), "B", "left"),
    panel_group(0:1, rnorm(2), "C", "left"),
    panel_group(2:3, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_line(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2HTML(viz)
  common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","x","y"))
})

test_that("should not make common chunk", {
  line_dt <- rbind(
    panel_group(c(0,1,NA,2,3), rnorm(5), "A", "left"),
    panel_group(c(NA,1,2), rnorm(3), "B", "left"),
    panel_group(c(0,NA,2), rnorm(3), "C", "left"),
    panel_group(3:4, rnorm(2), "A", "mid"),
    panel_group(2:3, rnorm(2), "B", "mid"),
    panel_group(4:5, rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2HTML(viz)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  expect_false(file.exists(common.tsv))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","na_group","row_in_group","x","y"))
})

test_that("x included in common chunk for path", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(0,NA,2), rnorm(3), "B", "left"),
    panel_group(c(NA,1,2), rnorm(3), "C", "left"),
    panel_group(c(3,NA,5), rnorm(3), "A", "mid"),
    panel_group(c(3,4,5),  rnorm(3), "B", "mid"),
    panel_group(c(5,6),  rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2HTML(viz)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_equal(nrow(chunk1.dt), 6)
  expect_identical(sort(names(chunk1.dt)), c("group","na_group","row_in_group","y"))
})

## TODO fix this test.
## test_that("x included in common chunk for line", {
##   line_dt <- rbind(
##     panel_group(c(0,1,NA), rnorm(3), "A", "left"),
##     panel_group(c(0,NA,2), rnorm(3), "B", "left"),
##     panel_group(c(NA,1,2), rnorm(3), "C", "left"),
##     panel_group(c(3,NA), rnorm(2), "A", "mid"),
##     panel_group(c(3,4),  rnorm(2), "B", "mid"),
##     panel_group(c(5,6),  rnorm(2), "A", "right"))
##   viz <- animint(
##     lines=ggplot()+
##       geom_line(aes(
##         x, y, group=g, color=g),
##         data=line_dt,
##         chunk_vars="panel",
##         showSelected="panel"))
##   info <- animint2HTML(viz)
##   common.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk_common.tsv")
##   common.dt <- fread(common.tsv)
##   expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
##   chunk1.tsv <- file.path(info$out.dir, "geom1_line_lines_chunk1.tsv")
##   chunk1.dt <- fread(chunk1.tsv)
##   expect_identical(sort(names(chunk1.dt)), c("group","y"))
## })

test_that("x not included in common chunk (mid has one group with 2 rows and another with 3)", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(0,NA,2), rnorm(3), "B", "left"),
    panel_group(c(NA,1,2), rnorm(3), "C", "left"),
    panel_group(c(3,4,5), rnorm(3), "A", "mid"),
    panel_group(c(3,4), rnorm(2), "B", "mid"),
    panel_group(c(5,6), rnorm(2), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel"))
  info <- animint2HTML(viz)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","x","y"))
  chunk2.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk2.tsv")
  chunk2.dt <- fread(chunk2.tsv)
  expect_identical(sort(names(chunk2.dt)), c("group","na_group","row_in_group","x","y"))
})

test_that("x included in common chunk (missing value in right)", {
  line_dt <- rbind(
    panel_group(c(0,1,NA), rnorm(3), "A", "left"),
    panel_group(c(0,NA,2), rnorm(3), "B", "left"),
    panel_group(c(NA,1,2), rnorm(3), "C", "left"),
    panel_group(c(3,4), rnorm(2), "A", "mid"),
    panel_group(c(3,4), rnorm(2), "B", "mid"),
    panel_group(c(4,5,NA,6,7), rnorm(5), "A", "right"))
  viz <- animint(
    lines=ggplot()+
      geom_path(aes(
        x, y, group=g, color=g),
        data=line_dt,
        chunk_vars="panel",
        showSelected="panel")+
      geom_point(aes(
        x, y, color=g),
        data=line_dt,
        showSelected="panel"))
  info <- animint2HTML(viz)
  path_nodes <- getNodeSet(info$html, "//g[@class='geom1_path_lines']//path")
  expect_equal(length(path_nodes), 4)
  common.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk_common.tsv")
  common.dt <- fread(common.tsv)
  expect_identical(sort(names(common.dt)), c("colour","group","showSelected2","x"))
  chunk1.tsv <- file.path(info$out.dir, "geom1_path_lines_chunk1.tsv")
  chunk1.dt <- fread(chunk1.tsv)
  expect_identical(sort(names(chunk1.dt)), c("group","na_group", "row_in_group", "y"))
})

