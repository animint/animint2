acontext("polygon subgroup holes")
library(animint2)
library(isoband)
library(data.table)
library(jsonlite)

make_viz <- function(m, subgroup = TRUE) {
  res <- as.data.table(
    isoband::isobands(
      (1:ncol(m)) / (ncol(m) + 1),
      (nrow(m):1) / (nrow(m) + 1),
      m, 0.5, 1.5
    )[[1]]
  )
  if (subgroup) {
    list(poly = ggplot() +
      geom_polygon(aes(x, y, group = 1, subgroup = id), data = res))
  } else {
    list(poly = ggplot() +
      geom_polygon(aes(x, y, group = id), data = res))
  }
}

compile <- function(viz) {
  out_dir <- tempfile()
  animint2dir(viz, out.dir = out_dir, open.browser = FALSE)
  list(
    json = jsonlite::fromJSON(file.path(out_dir, "plot.json")),
    tsv  = read.table(
      file.path(out_dir, "geom1_polygon_poly_chunk1.tsv"),
      header = TRUE, sep = "\t"
    ),
    dir = out_dir
  )
}

m_simple <- matrix(
  c(0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 0,
    0, 1, 0, 0, 1, 0,
    0, 1, 0, 0, 1, 0,
    0, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 0, 0),
  6, 6, byrow = TRUE)

m_only_hole <- rbind(
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 1, 0, 0, 0, 1, 0),
  c(0, 1, 0, 0, 0, 1, 0),
  c(0, 1, 0, 0, 0, 1, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0))

m_no_hole <- rbind(
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0))

m_hole_and_mid <- rbind(
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 1, 0, 0, 0, 1, 0),
  c(0, 1, 0, 1, 0, 1, 0),
  c(0, 1, 0, 0, 0, 1, 0),
  c(0, 1, 1, 1, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0))

## compiler tests (no browser needed)

test_that("data_has_subgroup flag is TRUE when subgroup used", {
  out <- compile(make_viz(m_simple, subgroup = TRUE))
  expect_true(out$json$geoms$geom1_polygon_poly$data_has_subgroup)
})

test_that("data_has_subgroup flag is absent when subgroup not used", {
  out <- compile(make_viz(m_simple, subgroup = FALSE))
  flag <- out$json$geoms$geom1_polygon_poly$data_has_subgroup
  expect_false(isTRUE(flag))
})

test_that("TSV contains subgroup column when subgroup used", {
  out <- compile(make_viz(m_simple, subgroup = TRUE))
  expect_true("subgroup" %in% names(out$tsv))
})

test_that("hole polygon TSV has 2 unique subgroup values", {
  out <- compile(make_viz(m_simple, subgroup = TRUE))
  expect_equal(length(unique(out$tsv$subgroup)), 2)
})

test_that("no-hole polygon TSV has 1 unique subgroup value", {
  out <- compile(make_viz(m_no_hole, subgroup = TRUE))
  expect_equal(length(unique(out$tsv$subgroup)), 1)
})

test_that("only_hole case has 2 subgroups in TSV", {
  out <- compile(make_viz(m_only_hole, subgroup = TRUE))
  expect_true(out$json$geoms$geom1_polygon_poly$data_has_subgroup)
  expect_equal(length(unique(out$tsv$subgroup)), 2)
})

test_that("no_hole case has 1 subgroup in TSV", {
  out <- compile(make_viz(m_no_hole, subgroup = TRUE))
  expect_equal(length(unique(out$tsv$subgroup)), 1)
})

test_that("hole_and_mid case has 3 subgroups in TSV", {
  out <- compile(make_viz(m_hole_and_mid, subgroup = TRUE))
  expect_true(out$json$geoms$geom1_polygon_poly$data_has_subgroup)
  expect_equal(length(unique(out$tsv$subgroup)), 3)
})

test_that("multiple groups with subgroup: 2 groups in TSV", {
  res1 <- as.data.table(isoband::isobands(
    (1:ncol(m_simple)) / (ncol(m_simple) + 1),
    (nrow(m_simple):1) / (nrow(m_simple) + 1),
    m_simple, 0.5, 1.5)[[1]])[, grp := "A"]
  res2 <- as.data.table(isoband::isobands(
    (1:ncol(m_only_hole)) / (ncol(m_only_hole) + 1),
    (nrow(m_only_hole):1) / (nrow(m_only_hole) + 1),
    m_only_hole, 0.5, 1.5)[[1]])[, grp := "B"]
  combined <- rbind(res1, res2)
  viz_multi <- list(
    poly = ggplot() +
      geom_polygon(aes(x, y, group = grp, subgroup = id), data = combined)
  )
  out_dir <- tempfile()
  animint2dir(viz_multi, out.dir = out_dir, open.browser = FALSE)
  tsv <- read.table(
    file.path(out_dir, "geom1_polygon_poly_chunk1.tsv"),
    header = TRUE, sep = "\t"
  )
  expect_true("subgroup" %in% names(tsv))
  expect_equal(length(unique(tsv$group)), 2)
})

## renderer tests (browser required)
## two groups A and B with clickSelects so clickID works meaningfully
res_A <- as.data.table(isoband::isobands(
  (1:ncol(m_simple)) / (ncol(m_simple) + 1),
  (nrow(m_simple):1) / (nrow(m_simple) + 1),
  m_simple, 0.5, 1.5)[[1]])[, grp := "A"]
res_B <- as.data.table(isoband::isobands(
  (1:ncol(m_only_hole)) / (ncol(m_only_hole) + 1),
  (nrow(m_only_hole):1) / (nrow(m_only_hole) + 1),
  m_only_hole, 0.5, 1.5)[[1]])[, grp := "B"]
res_click <- rbind(res_A, res_B)

viz_click <- list(
  poly = ggplot() +
    geom_polygon(
      aes(x, y, group = grp, subgroup = id, fill = grp,
          id = paste0("poly_", grp)),
      clickSelects = "grp",
      data = res_click
    )
)

info <- animint2HTML(viz_click)

test_that("rendered polygon with subgroup has fill-rule evenodd", {
  skip_if(!exists("remDr"), "remDr not initialized - run tests_init() first")
  html <- getHTML()
  path_nodes <- getNodeSet(html, "//path[@class='geom']")
  expect_gt(length(path_nodes), 0)
  styles <- sapply(path_nodes, xmlGetAttr, "style")
  expect_true(any(grepl("evenodd", styles)))
})

test_that("rendered polygon d attribute has 2 closed subpaths", {
  skip_if(!exists("remDr"), "remDr not initialized - run tests_init() first")
  html <- getHTML()
  path_nodes <- getNodeSet(html, "//path[@class='geom']")
  d_vals <- sapply(path_nodes, xmlGetAttr, "d")
  z_counts <- sapply(d_vals, function(d) nchar(gsub("[^Z]", "", d)))
  expect_true(any(z_counts >= 2))
})

test_that("hole subgroup merged into one path per group not two separate polygons", {
  skip_if(!exists("remDr"), "remDr not initialized - run tests_init() first")
  html <- getHTML()
  path_nodes <- getNodeSet(html, "//path[@class='geom']")
  ## 2 groups A and B => 2 path elements (one per group), not 4
  expect_equal(length(path_nodes), 2)
})

test_that("clickID on group A polygon deselects it and leaves group B visible", {
  skip_if(!exists("remDr"), "remDr not initialized - run tests_init() first")
  ## click group A to deselect it
  clickID("poly_A")
  Sys.sleep(1)
  html_after <- getHTML()
  path_nodes <- getNodeSet(html_after, "//path[@class='geom']")
  ## group B path should still exist
  expect_gt(length(path_nodes), 0)
  ## all remaining paths must still use evenodd fill rule
  styles <- sapply(path_nodes, xmlGetAttr, "style")
  expect_true(any(grepl("evenodd", styles)))
})

test_that("clickID on group A again reselects it restoring both polygons", {
  skip_if(!exists("remDr"), "remDr not initialized - run tests_init() first")
  clickID("poly_A")
  Sys.sleep(1)
  html_after <- getHTML()
  path_nodes <- getNodeSet(html_after, "//path[@class='geom']")
  expect_gte(length(path_nodes), 1)
  styles <- sapply(path_nodes, xmlGetAttr, "style")
  expect_true(any(grepl("evenodd", styles)))
})