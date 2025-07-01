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
  if(collect.coverage) {
    start_js_coverage()
  }
}
tests_run(filter=filter)
if(use.browser) {
  if(collect.coverage) {
    stop_js_coverage()
  }
  tests_exit()
}