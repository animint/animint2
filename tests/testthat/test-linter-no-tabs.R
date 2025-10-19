test_that("animint.js contains no tab characters", {
  animint_js_path <- system.file("htmljs", "animint.js", package = "animint2")
  if (animint_js_path == "") {
    # During R CMD check before installation
    animint_js_path <- "inst/htmljs/animint.js"
  }
  
  expect_true(file.exists(animint_js_path), 
              sprintf("animint.js not found at %s", animint_js_path))
  
  content <- readLines(animint_js_path, warn = FALSE)
  tab_lines <- grep("\t", content, perl = TRUE)
  
  if (length(tab_lines) > 0) {
    fail(sprintf("Tab characters found in animint.js at lines: %s. Please use 8 spaces instead of tabs.", 
                 paste(tab_lines, collapse = ", ")))
  }
  expect_length(tab_lines, 0)
})

