context("getCommonChunk")
## Pure unit test: no tests_init(), servr (port 4848), or browser required.
library(data.table)

expect_common_varied <- function(result, common_cols, varied_cols) {
  expect_type(result, "list")
  expect_named(result, c("common", "varied"))
  expect_true(all(common_cols %in% names(result$common)))
  expect_false(any(varied_cols %in% names(result$common)))
}

test_that("NULL when chunk.vars empty", {
  built <- data.table(x = 1:4, group = 1:4)
  expect_null(animint2:::getCommonChunk(built, character(), list(group = "group")))
})

test_that("NULL when only one chunk subset", {
  built <- data.table(
    x = 1:4,
    y = 5:8,
    group = 1:4,
    showSelected = 1
  )
  expect_null(animint2:::getCommonChunk(built, "showSelected", list(group = "group")))
})

test_that("NULL when no group appears in multiple subsets", {
  built <- data.table(
    x = 1:4,
    y = 5:8,
    group = 1:4,
    showSelected = 1:4
  )
  expect_null(animint2:::getCommonChunk(built, "showSelected", list(group = "group")))
})

test_that("polygon-like data splits x,y into common chunk", {
  ## Each group appears in showSelected 1 and 2; x,y fixed per group; fill varies.
  built <- data.table(
    group = rep(1:2, each = 4),
    showSelected = rep(c(1, 1, 2, 2), 2),
    x = rep(c(10, 20), each = 4),
    y = rep(c(1, 2), each = 4),
    fill = c("a", "a", "b", "b", "c", "c", "d", "d")
  )
  result <- animint2:::getCommonChunk(built, "showSelected", list(group = "group"))
  expect_common_varied(result, c("x", "y", "group"), "fill")
  expect_equal(nrow(result$common), 2)
})

test_that("colour common but xy varied", {
  built <- data.table(
    group = rep(c("left", "right"), each = 4),
    showSelected = rep(c("A", "A", "B", "B"), 2),
    x = 1:8,
    y = c(6, 6, 7, 7, 8, 8, 9, 9),
    colour = "foo",
    constant = "bar"
  )
  result <- animint2:::getCommonChunk(built, "showSelected", list(group = "group"))
  expect_common_varied(result, c("colour", "constant", "group"), c("x", "y"))
})

test_that("xy common but colour varied", {
  built <- data.table(
    group = rep(c("left", "right"), each = 4),
    showSelected = rep(c("A", "A", "B", "B"), 2),
    x = rep(c(1, 2), each = 4),
    y = rep(c(7, 8), each = 4),
    colour = c("red", "red", "blue", "blue", "green", "gold", "green", "gold")
  )
  result <- animint2:::getCommonChunk(built, "showSelected", list(group = "group"))
  expect_common_varied(result, c("x", "y", "group"), "colour")
})

test_that("single common column with multi-row group is allowed (#255)", {
  built <- data.table(
    group = rep(1:2, each = 4),
    showSelected = rep(c(1, 1, 2, 2), 2),
    x = c(10, 20, 10, 20, 30, 40, 30, 40),
    y = rnorm(8),
    colour = rnorm(8)
  )
  result <- animint2:::getCommonChunk(built, "showSelected", list(group = "group"))
  expect_common_varied(result, c("x", "group"), c("y", "colour"))
})

test_that("single common column with one row per group returns NULL", {
  built <- data.table(
    x = 1:4,
    y = rnorm(4),
    group = 1:4,
    colour = "red",
    showSelected = rep(1:2, each = 2)
  )
  expect_null(animint2:::getCommonChunk(built, "showSelected", list(group = "group")))
})

test_that("NA paths keep na_group and row_in_group in varied data", {
  built <- data.table(
    group = rep(1:2, each = 4),
    showSelected = rep(c(1, 1, 2, 2), 2),
    x = c(NA, 2, NA, 2, NA, 5, NA, 5),
    y = 1:8,
    na_group = c(0, 0, 1, 0, 0, 0, 1, 0),
    row_in_group = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  result <- animint2:::getCommonChunk(built, "showSelected", list(group = "group"))
  expect_type(result, "list")
  chunk1 <- result$varied[["1"]]
  expect_true(all(c("na_group", "row_in_group", "y") %in% names(chunk1)))
  expect_false("x" %in% names(chunk1))
})

test_that("factors are treated as character", {
  built <- data.table(
    group = rep(1:2, each = 4),
    showSelected = factor(rep(c(1, 1, 2, 2), 2)),
    x = rep(c(10, 20), each = 4),
    y = rep(c(1, 2), each = 4),
    fill = factor(c("a", "a", "b", "b", "c", "c", "d", "d"))
  )
  result <- animint2:::getCommonChunk(built, "showSelected", list(group = "group"))
  expect_common_varied(result, c("x", "y", "group"), "fill")
})

test_that("C++ and R detect_common_value_dt agree", {
  built <- data.table(
    group = rep(1:2, each = 4),
    showSelected = rep(c(1, 1, 2, 2), 2),
    x = rep(c(10, 20), each = 4),
    y = rep(c(1, 2), each = 4),
    colour = rep(c("r", "g"), each = 4),
    na_group = 0,
    row_in_group = 1:4
  )
  chunk.vars <- "showSelected"
  col.name.vec <- c("x", "y", "colour")
  setkeyv(built, c("group", chunk.vars))
  r_dt <- with(
    options(animint2.use.cpp = FALSE),
    animint2:::detect_common_value_dt(built, col.name.vec, chunk.vars)
  )
  if(exists("common_value_for_group_subset_cpp", where = asNamespace("animint2"), mode = "function")){
    cpp_dt <- animint2:::detect_common_value_dt(built, col.name.vec, chunk.vars)
    setorder(r_dt, col.name, group)
    setorder(cpp_dt, col.name, group)
    expect_equal(nrow(r_dt), nrow(cpp_dt))
    expect_equal(r_dt$col.name, cpp_dt$col.name)
    expect_equal(r_dt$group, cpp_dt$group)
    expect_equal(r_dt$is.common, cpp_dt$is.common)
    for(i in seq_len(nrow(r_dt))){
      expect_equal(r_dt$common[[i]], cpp_dt$common[[i]])
    }
  } else {
    skip("C++ not compiled")
  }
})
