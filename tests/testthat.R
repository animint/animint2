library("testthat")
library("animint2")
library("XML")
setwd("testthat")
source("helper-functions.R")
source("helper-HTML.R")
source("helper-plot-data.r")
gh.action <- Sys.getenv("GH_ACTION")
is_js_coverage <- Sys.getenv("TEST_SUITE") == "JS_coverage"
message(gh.action)
tests_init()
# Start coverage if enabled
coverage_active <- FALSE
if(is_js_coverage) {
  coverage_active <- start_js_coverage()
  if(coverage_active) {
    message("JS coverage collection started")
  }
}
# Run tests
message("\n=== Running COMPILER tests ===")
tests_run(filter = "compiler")
message("\n=== Running RENDERER tests ===")
tests_run(filter = "renderer")
# Save coverage and cleanup
if(coverage_active) {
  stop_js_coverage()
}
message("\n=== Running SHINY tests ===")
tests_run(filter = "shiny")
tests_exit()