#' Scale point sizes using circle area, but specifying the radius in
#' pixels.
#' @param pixel.range min and max circle radius in pixels.
#' @param ... passed to continuous_a_scale.
#' @export
a_scale_size_animint <- function(pixel.range=c(2,20), ...){
  continuous_a_scale("size","area",palette=function(x){
    scales::rescale(sqrt(abs(x)), pixel.range, c(0,1))
  },...)
}
