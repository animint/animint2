## General helper functions

#' Check if character is an RGB hexadecimal color value
#' @param x character
#' @return True/False value
#' @export
is.rgb <- function(x){
  if(is.null(x)) {
    TRUE
  } else {
    (grepl("#", x) & nchar(x)==7)
  }
}

#' Convert R colors to RGB hexadecimal color values
#' @param x character
#' @return hexadecimal color value or "transparent" if is.na
#' @export
toRGB <- function(x){
  is.transparent <- is.na(x) | x=="transparent"
  rgb.mat <- col2rgb(x)
  rgb.vec <- rgb(t(rgb.mat), maxColorValue=255)
  named.vec <- ifelse(is.transparent, "transparent", rgb.vec)
  not.named <- as.character(named.vec)
  not.named
}
