library(jsonlite)

# Helper: map URL to local file path
url_to_local_path <- function(url) {
  # Adjust this mapping as needed for your setup
  if (grepl("animint-htmltest/animint.js", url)) {
    return("inst/htmljs/animint.js")
  }
  # Add more mappings if you have more JS files
  return(NULL)
}

convert_js_to_lcov <- function(input_file = "tests/testthat/js-coverage.json", 
                               output_file = "js-coverage.lcov") {
  if (!file.exists(input_file)) stop(paste("Input file not found:", input_file))
  coverage_data <- fromJSON(input_file, simplifyDataFrame = FALSE)
  lcov_content <- character()
  
  for (script in coverage_data$result) {
    url <- script$url
    local_path <- url_to_local_path(url)
    if (is.null(local_path) || !file.exists(local_path)) next
    
    # Read file and compute line start/end byte offsets
    file_lines <- readLines(local_path, warn = FALSE)
    line_starts <- cumsum(c(0, nchar(file_lines, type = "bytes") + 1)) # +1 for \n
    line_ends <- line_starts[-1] - 1
    line_ends[length(line_ends)] <- line_ends[length(line_ends)] + 1 # last line
    
    # For each line, sum up coverage counts from all overlapping ranges
    line_counts <- integer(length(file_lines))
    for (func in script$functions) {
      for (range in func$ranges) {
        # Find all lines that overlap with this range
        covered_lines <- which(
          (line_starts < range$endOffset) & (line_ends > range$startOffset)
        )
        for (ln in covered_lines) {
          line_counts[ln] <- line_counts[ln] + range$count
        }
      }
    }
    
    # Write LCOV for this file
    lcov_content <- c(lcov_content, paste0("SF:", local_path))
    for (ln in seq_along(line_counts)) {
      lcov_content <- c(lcov_content, sprintf("DA:%d,%d", ln, line_counts[ln]))
    }
    lcov_content <- c(
      lcov_content,
      sprintf("LH:%d", sum(line_counts > 0)),
      sprintf("LF:%d", length(line_counts)),
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