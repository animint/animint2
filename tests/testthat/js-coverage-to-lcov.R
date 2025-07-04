library(jsonlite)

# Recursively list all JS files in a directory
list_js_files <- function(dir) {
  files <- list.files(dir, pattern = "\\.(js|css)$", recursive = TRUE, full.names = TRUE)
  normalizePath(files, winslash = "/", mustWork = FALSE)
}

# Map coverage JSON URLs to local file paths
url_to_local_path <- function(url) {
  m <- regexpr("animint-htmltest/([a-zA-Z0-9_./-]+\\.js)$", url)
  if (m != -1) {
    rel <- regmatches(url, m)
    rel <- sub("animint-htmltest/", "", rel)
    local <- file.path("inst/htmljs", rel)
    if (file.exists(local)) return(normalizePath(local, winslash = "/"))
  }
  return(NULL)
}

# Main conversion function
convert_js_to_lcov <- function(input_file = "tests/testthat/js-coverage.json", 
                               output_file = "js-coverage.lcov") {
  all_js_files <- list_js_files("inst/htmljs")
  all_js_files <- sort(all_js_files)
  covered_files <- list()
  if (file.exists(input_file)) {
    coverage_data <- fromJSON(input_file, simplifyDataFrame = FALSE)
    for (script in coverage_data$result) {
      local_path <- url_to_local_path(script$url)
      if (!is.null(local_path) && file.exists(local_path)) {
        covered_files[[local_path]] <- script
      }
    }
  }
  lcov_content <- character()
  for (js_file in all_js_files) {
    file_lines <- readLines(js_file, warn = FALSE)
    n_lines <- length(file_lines)
    line_covered <- rep(0L, n_lines)
    script <- covered_files[[js_file]]
    if (!is.null(script)) {
      # Compute byte offsets for each line
      line_starts <- cumsum(c(0, nchar(file_lines, type = "bytes") + 1)) # +1 for \n
      line_ends <- line_starts[-1] - 1
      line_ends[length(line_ends)] <- line_ends[length(line_ends)] + 1 # last line
      for (func in script$functions) {
        for (range in func$ranges) {
          if (range$count > 0) {
            # Mark all lines that overlap with this range as covered
            covered_lines <- which(
              (line_starts < range$endOffset) & (line_ends > range$startOffset)
            )
            line_covered[covered_lines] <- 1L
          }
        }
      }
    }
    # Write LCOV for this file
    lcov_content <- c(lcov_content, paste0("SF:", js_file))
    for (ln in seq_along(line_covered)) {
      lcov_content <- c(lcov_content, sprintf("DA:%d,%d", ln, line_covered[ln]))
    }
    lcov_content <- c(
      lcov_content,
      sprintf("LH:%d", sum(line_covered > 0)),
      sprintf("LF:%d", length(line_covered)),
      "end_of_record"
    )
  }
  writeLines(lcov_content, output_file)
  message(sprintf("Wrote LCOV to %s", output_file))
}

# Run conversion
tryCatch({
  convert_js_to_lcov()
  quit(status = 0)
}, error = function(e) {
  message(paste("Error:", e$message))
  quit(status = 1)
})