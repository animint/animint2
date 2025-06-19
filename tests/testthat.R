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
if(filter == ""){
  filter <- NULL
}
message(gh.action)
if(use.browser) {
  tests_init()
  start_js_coverage() # <-- Start JS coverage
}
tests_run(filter=filter)
if(use.browser) {
  stop_js_coverage("../js-coverage.json") # <-- Save JS coverage
  tests_exit()
}
