#' Convert newline characters to HTML line breaks for text rendering
#'
#' @description
#' This function converts literal newline characters (`\n`) in text strings
#' to HTML `<br/>` tags. This enables multi-line text rendering in various
#' plot elements including tooltips, axis titles, plot titles, legend entries,
#' and geom_text labels.
#'
#' @details
#' animint2 renders text in two contexts:
#' 
#' 1. **HTML context** (tooltips): Uses `<br/>` for line breaks
#' 2. **SVG context** (titles, labels, legends): Also uses `<br/>` as a marker
#'    that will be converted to `<tspan>` elements by JavaScript
#'
#' This function provides a centralized conversion that works for all text
#' elements in animint2 visualizations, as suggested in issue #221.
#'
#' @param text Character vector or NULL. Text string(s) that may contain
#'   newline characters (`\n`).
#'
#' @return Character vector with `\n` replaced by `<br/>`, or NULL if input
#'   is NULL. Preserves the length and names of the input vector.
#'
#' @examples
#' # Single string
#' convertNewlinesToBreaks("First line\nSecond line")
#' # Returns: "First line<br/>Second line"
#' 
#' # Vector of strings
#' convertNewlinesToBreaks(c("One\nTwo", "Single", "A\nB\nC"))
#' # Returns: c("One<br/>Two", "Single", "A<br/>B<br/>C")
#' 
#' # NULL input
#' convertNewlinesToBreaks(NULL)
#' # Returns: NULL
#' 
#' # Empty string
#' convertNewlinesToBreaks("")
#' # Returns: ""
#'
#' @seealso
#' Used internally by:
#' - `compileLayer()` for geom_text labels
#' - `getLegend()` for legend entries
#' - `getPlotInfo()` for axis and plot titles
#'
#' @keywords internal
convertNewlinesToBreaks <- function(text) {
  # Handle NULL input
  if (is.null(text)) {
    return(NULL)
  }
  
  # Handle empty vectors
  if (length(text) == 0) {
    return(character(0))
  }
  
  # Validate input type
  if (!is.character(text) && !is.factor(text)) {
    warning(
      "convertNewlinesToBreaks expects character or factor input, got ",
      class(text)[1],
      ". Converting to character."
    )
    text <- as.character(text)
  }
  
  # Convert factors to character to avoid issues
  if (is.factor(text)) {
    text <- as.character(text)
  }
  
  # Preserve names if present
  original_names <- names(text)
  
  # Perform the conversion: \n -> <br/>
  # Using gsub with fixed=FALSE to handle literal \n in the string
  result <- gsub("\n", "<br/>", text, fixed = TRUE)
  
  # Restore names
  names(result) <- original_names
  
  return(result)
}


#' Check if text contains newline characters
#'
#' @description
#' Helper function to quickly check if any strings in a character vector
#' contain newline characters that would benefit from conversion.
#'
#' @param text Character vector to check
#'
#' @return Logical scalar indicating whether any element contains `\n`
#'
#' @examples
#' hasNewlines("Regular text")  # FALSE
#' hasNewlines("Text\nwith newline")  # TRUE
#' hasNewlines(c("First", "Second\nLine"))  # TRUE
#'
#' @keywords internal
hasNewlines <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(FALSE)
  }
  any(grepl("\n", text, fixed = TRUE))
}
