context("URL hash handling")

test_that("animint.js does not contain old hash parsing code", {
  js_file <- system.file("htmljs", "animint.js", package = "animint2")
  js_lines <- readLines(js_file)
  has_old_code <- any(grepl("if\\s*\\(\\s*window\\.location\\.hash\\s*\\)", js_lines))
  expect_false(
    has_old_code,
    info = "Old window.location.hash parsing code should be removed (issue #280)"
  )
})
