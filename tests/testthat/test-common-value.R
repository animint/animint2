test_that("common_value_for_chunks matches the three R cases", {
  expect_same <- function(value_list, is_common, common) {
    ans <- animint2:::common_value_for_chunks(value_list)
    expect_equal(ans$is.common, is_common)
    expect_equal(ans$common, common)
  }
  expect_same(list(c(10, 20), c(10, 20)), TRUE, list(c(10, 20)))
  expect_same(list(c(10, 20), c(10, 21)), FALSE, list(c(10, 20)))
  expect_same(list(c(10, NA), c(NA, 10)), TRUE, list(10))
  expect_same(list(c(1, 2), 1), FALSE, list())
  expect_same(list(c("a", "b"), c("a", "b")), TRUE, list(c("a", "b")))
  expect_same(list(1L, 1L), TRUE, list(1L))
})

test_that("getCommonChunk still separates common and varied columns", {
  built <- data.table::data.table(
    group = rep(1:2, each = 4),
    showSelected = rep(c(1, 1, 2, 2), 2),
    x = rep(c(10, 20), each = 4),
    y = rep(c(1, 2), each = 4),
    fill = c("a", "a", "b", "b", "c", "c", "d", "d")
  )
  data.table::setkeyv(built, c("group", "showSelected"))
  result <- animint2:::getCommonChunk(
    built, "showSelected", list(group = "group"))
  expect_equal(names(result$common), c("group", "x", "y"))
  expect_equal(result$common$x, c(10, 20))
  expect_equal(names(result$varied[["1"]]), c("group", "fill"))
})
