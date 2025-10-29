test_that("download status table should show file sizes", {
  js_file = system.file("htmljs", "animint.js", package="animint2")
  js_text = paste(readLines(js_file), collapse=" ")
  mb_columns = grep("total_MB", js_text)
  expect_gt(length(mb_columns), 0)
})
