# Generate R coverage as Cobertura XML for a later combined Codecov upload.
# Path cleanup uses gsub() so Codecov can match files in this repository.
if (!requireNamespace("covr", quietly = TRUE)) {
  stop("covr is required to generate R coverage")
}

cov <- covr::package_coverage(
  quiet = FALSE,
  type = "tests",
  test_files = "tests/testthat.R"
)
print(cov)

out_file <- "r-coverage.xml"
covr::to_cobertura(cov, filename = out_file)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
xml <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
xml <- gsub("\\\\", "/", xml)
xml <- gsub(root, ".", xml, fixed = TRUE)
writeLines(xml, out_file)

message("Wrote ", out_file)
