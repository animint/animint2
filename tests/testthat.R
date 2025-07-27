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
  if(collect.coverage) {
    options(covr.record_tests = TRUE)
    message("Starting JS coverage collection...")
    coverage_active <- start_js_coverage()
    cov <- covr::package_coverage(
      type = "none",
      quiet = FALSE   # Show test output
    )
    # Run tests normally - they'll be recorded by covr
    message("\n=== Running COMPILER tests ===")
    tests_run(filter = "compiler")
    message("\n=== Running RENDERER tests ===")
    tests_run(filter = "renderer")
    # Save coverage
    cov <- covr::package_coverage()
  } else {
    message("\n=== Running COMPILER tests ===")
    tests_run(filter = "compiler")
    message("\n=== Running RENDERER tests ===")
    tests_run(filter = "renderer")
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