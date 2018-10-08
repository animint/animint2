## General helper functions


#' Convert \code{pt} value to \code{lines}
#' 
#' @param pt_value Value in \code{pt} to be converted to \code{lines}
#' @return Value in \code{lines}
#' @note Does NOT work if input is not in \code{pt}. Input is returned as is.
pt.to.lines <- function(pt_value){
  if(attributes(pt_value)$unit == "pt"){
    pt_value <- round(as.numeric(pt_value) * (0.25/5.5), digits = 2)
  }
  as.numeric(pt_value)
}


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
#' @importFrom grDevices col2rgb rgb
#' @export
toRGB <- function(x){
  is.transparent <- is.na(x) | x=="transparent"
  rgb.mat <- col2rgb(x)
  rgb.vec <- rgb(t(rgb.mat), maxColorValue=255)
  named.vec <- ifelse(is.transparent, "transparent", rgb.vec)
  not.named <- as.character(named.vec)
  not.named
}
