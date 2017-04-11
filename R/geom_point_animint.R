GeomPointAnimint <- ggplot2::ggproto("GeomPointAnimint", GeomPoint,
                             default_aes = aes(
                               shape  = 19,
                               colour = "black",
                               size   = 1.5,
                               fill   = NA,
                               alpha  = NA,
                               stroke = 0.5,
                               chunk_vars = NA,
                               clickSelects = NA,
                               showSelected = NA
                               )
)

#' @export
geom_point_animint <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                                ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){
  
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomPointAnimint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}
