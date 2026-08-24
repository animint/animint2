# Generate the same Rlang JSON that covr::codecov() used to POST, then save it
# as an artifact for a later combined Codecov upload.
if (!requireNamespace("covr", quietly = TRUE)) {
  stop("covr is required to generate R coverage")
}

cov <- covr::package_coverage(
  quiet = FALSE,
  type = "tests",
  test_files = "tests/testthat.R"
)
print(cov)

out_file <- "r-coverage.json"
# to_codecov() is the JSON body that covr::codecov() uploads.
writeLines(covr:::to_codecov(cov), out_file)

message("Wrote ", out_file)
