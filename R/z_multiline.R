convertNewlinesToBreaks <- function(text) {
  gsub("\n", "<br/>", text, fixed = TRUE)
}

