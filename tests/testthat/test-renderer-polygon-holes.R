library(testthat)
library(animint2)
library(XML)
context("Polygon holes via subgroup aesthetic")
tests_init()

## ---- data setup ----

## simple donut: outer ring (id=1) + hole (id=2)
make.hole.data <- function(){
  m <- matrix(c(
    0,0,0,0,0,0,
    0,1,1,1,1,0,
    0,1,0,0,1,0,
    0,1,0,0,1,0,
    0,1,1,1,1,0,
    0,0,0,0,0,0), 6, 6, byrow=TRUE)
  res <- isoband::isobands(
    (1:ncol(m))/(ncol(m)+1),
    (nrow(m):1)/(nrow(m)+1),
    m, 0.5, 1.5)[[1]]
  as.data.frame(res)
}

## full test case from issue: 3 polygon types side by side
## hole_and_mid: outer ring + hole + island (3 subgroups)
## only_hole:    outer ring + hole (2 subgroups)
## no_hole:      outer ring only (1 subgroup)
make.full.data <- function(){
  m.list <- list(
    hole_and_mid=rbind(
      c(0,0,0,0,0,0,0),
      c(0,1,1,1,1,1,0),
      c(0,1,0,0,0,1,0),
      c(0,1,0,1,0,1,0),
      c(0,1,0,0,0,1,0),
      c(0,1,1,1,1,1,0),
      c(0,0,0,0,0,0,0)),
    only_hole=rbind(
      c(0,0,0,0,0,0,0),
      c(0,1,1,1,1,1,0),
      c(0,1,0,0,0,1,0),
      c(0,1,0,0,0,1,0),
      c(0,1,0,0,0,1,0),
      c(0,1,1,1,1,1,0),
      c(0,0,0,0,0,0,0)),
    no_hole=rbind(
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
    offset   <- grp.i * 10
    grp.name <- names(m.list)[[grp.i]]
    m        <- m.list[[grp.i]]
    iband    <- isoband::isobands(
      1:ncol(m), nrow(m):1, m, 0.5, 1.5)[[1]]
    poly.df       <- as.data.frame(iband)
    poly.df$grp   <- grp.name
    poly.df$x     <- poly.df$x + offset
    poly.list[[grp.i]] <- poly.df
    point.list[[grp.i]] <- data.frame(
      x     = c(4,5,6,7) + offset,
      y     = 4,
      label = paste0(grp.name, "_", c("mid","hole","ring","out")))
  }
  list(
    poly.dt  = do.call(rbind, poly.list),
    point.dt = do.call(rbind, point.list))
}

hole.data <- make.hole.data()
full.data <- make.full.data()

## ---- visualizations ----

viz.simple <- list(
  poly=ggplot()+
    geom_polygon(
      aes(x, y, group=1, subgroup=id),
      data=hole.data,
      fill="steelblue")+
    theme_animint(width=400, height=400)
)

viz.full <- list(
  poly=ggplot()+
    geom_polygon(
      aes(x, y, group=grp, subgroup=id),
      data=full.data$poly.dt,
      fill="steelblue")+
    geom_point(
      aes(x, y, id=label),
      data=full.data$point.dt,
      color="red", size=3)+
    theme_animint(width=700, height=400)
)

## ---- compiler tests (no browser needed) ----

test_that("compiler: subgroup column appears in TSV output", {
  out.dir <- tempfile()
  animint2dir(viz.simple, out.dir=out.dir, open.browser=FALSE)
  tsv.files <- list.files(out.dir, pattern="geom.*\\.tsv$", full.names=TRUE)
  expect_true(length(tsv.files) > 0)
  tsv.df <- read.delim(tsv.files[[1]])
  expect_true(
    "subgroup" %in% names(tsv.df),
    info=paste("columns found:", paste(names(tsv.df), collapse=", ")))
})

test_that("compiler: data_has_subgroup flag written to plot.json", {
  out.dir <- tempfile()
  animint2dir(viz.simple, out.dir=out.dir, open.browser=FALSE)
  json.txt <- paste(readLines(file.path(out.dir, "plot.json"), warn=FALSE), collapse="")
  expect_true(
    grepl("data_has_subgroup", json.txt),
    info="plot.json must contain data_has_subgroup flag")
})

test_that("compiler: no subgroup flag when subgroup aes not used", {
  viz.plain <- list(
    poly=ggplot()+
      geom_polygon(
        aes(x, y, group=id),
        data=hole.data[hole.data$id==1, ]))
  out.dir <- tempfile()
  animint2dir(viz.plain, out.dir=out.dir, open.browser=FALSE)
  json.txt <- paste(readLines(file.path(out.dir, "plot.json"), warn=FALSE), collapse="")
  expect_false(
    grepl("data_has_subgroup.*true", json.txt, ignore.case=TRUE),
    info="data_has_subgroup should not appear when subgroup not used")
})

## ---- renderer tests (requires Chrome via chromote) ----

info <- animint2HTML(viz.simple)

test_that("renderer: SVG renders without error", {
  expect_true(!is.null(info))
  expect_true(grepl("<svg", saveXML(getHTML())))
})

test_that("renderer: SVG path element used for polygon with subgroup", {
  html      <- getHTML()
  path.list <- getNodeSet(html, '//g[contains(@class,"geom")]//path')
  expect_true(
    length(path.list) > 0,
    info="subgroup polygon must render as SVG <path>, not <polygon>")
})

test_that("renderer: path d attribute has multiple M commands for hole rings", {
  html      <- getHTML()
  path.list <- getNodeSet(html, '//g[contains(@class,"geom")]//path')
  expect_true(length(path.list) > 0)
  d.vals <- sapply(path.list, function(node) xmlGetAttr(node, "d"))
  d.vals <- d.vals[nchar(d.vals) > 0]
  ## a hole polygon needs >= 2 M commands: one per ring (outer + hole)
  has.multi.M <- any(sapply(d.vals, function(d){
    length(gregexpr("M", d)[[1]]) >= 2
  }))
  expect_true(has.multi.M,
    info="hole polygon path 'd' must contain >= 2 M commands (one per ring)")
})

test_that("renderer: evenodd fill-rule applied to polygon path", {
  html      <- getHTML()
  path.list <- getNodeSet(html, '//g[contains(@class,"geom")]//path')
  expect_true(length(path.list) > 0)
  style.vals <- sapply(path.list, function(node) xmlGetAttr(node, "style"))
  expect_true(
    any(grepl("evenodd", style.vals, ignore.case=TRUE)),
    info=paste("fill-rule:evenodd not found. styles:",
               paste(style.vals, collapse="; ")))
})

## ---- interactive tests  ----

info.full <- animint2HTML(viz.full)

test_that("interactive: full viz with 3 polygon types renders", {
  expect_true(!is.null(info.full))
  expect_true(grepl("<svg", saveXML(getHTML())))
})

test_that("interactive: clickID inside hole does not change polygon path count", {
  html.before  <- getHTML()
  count.before <- length(getNodeSet(html.before,
    '//g[contains(@class,"geom")]//path'))

  ## click the red point that sits inside the hole of only_hole polygon
  clickID("only_hole_hole")
  Sys.sleep(2)

  html.after  <- getHTML()
  count.after <- length(getNodeSet(html.after,
    '//g[contains(@class,"geom")]//path'))

  ## path count must be unchanged , clicking inside a hole
  ## should not add or remove polygon path elements
  expect_equal(count.before, count.after)
})

test_that("interactive: all rendered path elements have non-empty d attribute", {
  html      <- getHTML()
  path.list <- getNodeSet(html, '//g[contains(@class,"geom")]//path')
  expect_true(length(path.list) >= 1)
  d.vals <- sapply(path.list, function(node) xmlGetAttr(node, "d"))
  expect_true(
    all(nchar(d.vals) > 0),
    info="every path element must have a non-empty 'd' attribute")
})

test_that("interactive: no_hole polygon renders as single-ring path", {
  html      <- getHTML()
  path.list <- getNodeSet(html, '//g[contains(@class,"geom")]//path')
  d.vals    <- sapply(path.list, function(node) xmlGetAttr(node, "d"))
  ## no_hole has only 1 subgroup so its path should have exactly 1 M command
  has.single.M <- any(sapply(d.vals, function(d){
    length(gregexpr("M", d)[[1]]) == 1
  }))
  expect_true(has.single.M,
    info="no_hole polygon path should have exactly 1 M command")
})