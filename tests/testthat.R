library("testthat")
library("animint2")
library("XML")
setwd("testthat")
source("helper-functions.R")
source("helper-HTML.R")
source("helper-plot-data.r")
filter <- Sys.getenv("TEST_SUITE")
gh.action <- Sys.getenv("GH_ACTION")
use.browser <- grepl("renderer", filter)
collect.coverage <- Sys.getenv("COLLECT_COVERAGE", "FALSE") == "TRUE"
if(filter == ""){
  filter <- NULL
}
message(gh.action)
if(use.browser) {
  tests_init()
  coverage_active <- start_js_coverage()
  if(exists("coverage_active") && coverage_active) {
      print("JS coverage is active")
    }
  on.exit({
    if(exists("coverage_active") && coverage_active) {
      stop_js_coverage()
    }
    tests_exit()
  }, add = TRUE)
}

tests_run(filter=filter)