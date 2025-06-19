# library(covr)
# # Exclude deprecated or non-package code if needed:
# # coverage <- package_coverage(path = ".", type = "tests", exclusions = "R/deprecated/")
# coverage <- package_coverage(path = ".", type = "tests")
# codecov(coverage)
library(covr)
# Exclude helper files and testthat infrastructure
cov <- package_coverage(path = ".", type = "tests", relative_path = TRUE)
# Save as XML for Codecov
covr::to_cobertura(cov, file = "coverage.xml")