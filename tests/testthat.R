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
  if(collect.coverage) {
    message("Starting JS coverage collection...")
    coverage_active <- start_js_coverage()
  }
  run_all_tests <- function() {
    message("\n=== Running COMPILER tests ===")
    tests_run(filter = "compiler")
    message("\n=== Running RENDERER tests ===")
    tests_run(filter = "renderer")
  }
  # Run tests with coverage tracking
  if(collect.coverage) {
    cov <- covr::environment_coverage(
      env = environment(),
      test_files = NULL,
      code = run_all_tests()
    )
  } else {
    run_all_tests()
  }
  # Save JS coverage
  if(collect.coverage && exists("coverage_active") && coverage_active) {
    stop_js_coverage()
    message("JS coverage saved")
  }
  tests_exit()
  # Save R coverage
  if(collect.coverage) {
    message("Uploading R coverage...")
    covr::codecov(cov, quiet = FALSE, flags = "r")
  }
}