test_that("download status table should show file sizes", {
  js_file <- system.file("htmljs", "animint.js", package="animint2")
  js_text <- paste(readLines(js_file), collapse=" ")
  cat("\n=== Issue #249: Download status table size info ===\n")
  has_mb_columns <- grepl("total_MB|mean_MB", js_text)
  cat(sprintf("animint.js has MB column code: %s\n", has_mb_columns))
  expect_true(has_mb_columns, label="animint.js should generate total_MB and mean_MB columns in download status table")
})
