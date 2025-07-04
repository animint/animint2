library(jsonlite)

# Recursively list all JS files in a directory
list_js_files <- function(dir) {
  files <- list.files(dir, pattern = "\\.js$", recursive = TRUE, full.names = TRUE)
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

# Calculate line start/end byte offsets for a file
get_line_offsets <- function(file_lines) {
  line_lengths <- nchar(file_lines, type = "bytes")
  cum_lengths <- cumsum(line_lengths + 1) # +1 for newline
  data.frame(
    start = c(0, cum_lengths[-length(cum_lengths)]),
    end = cum_lengths - 1
  )
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
    line_offsets <- get_line_offsets(file_lines)
    n_lines <- length(file_lines)
    # Initialize coverage tracking
    line_hits <- rep(0L, n_lines) 
    script <- covered_files[[js_file]]
    if (!is.null(script)) {
      # Process each executed range
      for (func in script$functions) {
        for (range in func$ranges) {
          if (range$count > 0) {
            # Find lines that are fully contained within this range
            for (ln in seq_len(n_lines)) {
              line_start <- line_offsets$start[ln]
              line_end <- line_offsets$end[ln]
              # Only count if the entire line is within the executed range
              if (range$startOffset <= line_start && range$endOffset >= line_end) {
                line_hits[ln] <- line_hits[ln] + range$count
              }
            }
          }
        }
      }
    }
    # Write LCOV for this file
    lcov_content <- c(lcov_content, paste0("SF:", js_file))
    # Report line coverage
    for (ln in seq_len(n_lines)) {
      lcov_content <- c(lcov_content, sprintf("DA:%d,%d", ln, line_hits[ln]))
    }
    lcov_content <- c(
      lcov_content,
      sprintf("LH:%d", sum(line_hits > 0)),
      sprintf("LF:%d", n_lines),
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