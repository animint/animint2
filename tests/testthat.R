library("testthat")
library("animint2")
library("XML")
setwd("testthat")
source("helper-functions.R")
source("helper-HTML.R")
source("helper-plot-data.r")
filter <- Sys.getenv("TEST_SUITE")
gh.action <- Sys.getenv("GH_ACTION")
collect.coverage <- Sys.getenv("COLLECT_COVERAGE", "FALSE") == "TRUE"
is.cran <- Sys.getenv("TEST_SUITE") == "CRAN"
if(filter == ""){
  filter <- NULL
}
message(gh.action)

if(!is.cran) {
  tests_init()
  # Start coverage if enabled
  coverage_active <- FALSE
  if(collect.coverage) {
    coverage_active <- start_js_coverage()
    if(coverage_active) {
      message("JS coverage collection started")
    }
  }
  # Run tests
  tests_run()
  # Save coverage and cleanup
  if(coverage_active) {
    stop_js_coverage()
  }
  tests_exit()
}