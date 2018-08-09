#' Line segments and curves.
#'
#' \code{a_geom_segment} draws a straight line between points (x1, y1) and
#' (x2, y2). \code{a_geom_curve} draws a curved line.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "segment")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param arrow specification for arrow heads, as created by arrow()
#' @param lineend Line end style (round, butt, square)
#' @seealso \code{\link{a_geom_path}} and \code{\link{a_geom_line}} for multi-
#'   segment lines and paths.
#' @seealso \code{\link{a_geom_spoke}} for a segment parameterised by a location
#'   (x, y), and an angle and radius.
#' @export
#' @examples
#' b <- a_plot(mtcars, a_aes(wt, mpg)) +
#'   a_geom_point()
#'
#' df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
#' b +
#'  a_geom_curve(a_aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df) +
#'  a_geom_segment(a_aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)
#'
#' b + a_geom_curve(a_aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = -0.2)
#' b + a_geom_curve(a_aes(x = x1, y = y1, xend = x2, yend = y2), data = df, curvature = 1)
#' b + a_geom_curve(
#'   a_aes(x = x1, y = y1, xend = x2, yend = y2),
#'   data = df,
#'   arrow = arrow(length = unit(0.03, "npc"))
#' )
#'
#' a_plot(seals, a_aes(long, lat)) +
#'   a_geom_segment(a_aes(xend = long + delta_long, yend = lat + delta_lat),
#'     arrow = arrow(length = unit(0.1,"cm"))) +
#'     borders("state")
#'
#' # You can also use a_geom_segment to recreate plot(type = "h") :
#' counts <- as.data.frame(table(x = rpois(100,5)))
#' counts$x <- as.numeric(as.character(counts$x))
#' with(counts, plot(x, Freq, type = "h", lwd = 10))
#'
#' a_plot(counts, a_aes(x, Freq)) +
#'   a_geom_segment(a_aes(xend = x, yend = 0), size = 10, lineend = "butt")
a_geom_segment <- function(mapping = NULL, data = NULL,
                         a_stat = "identity", a_position = "identity",
                         ...,
                         arrow = NULL,
                         lineend = "butt",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomSegment,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomSegment <- a_ggproto("a_GeomSegment", a_Geom,
  required_aes = c("x", "y", "xend", "yend"),
  non_missing_aes = c("linetype", "size", "shape"),
  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_panel = function(data, panel_scales, a_coord, arrow = NULL,
                        lineend = "butt", na.rm = FALSE) {

    data <- remove_missing(data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "size", "shape"),
      name = "a_geom_segment")
    if (empty(data)) return(a_zeroGrob())

    if (a_coord$is_linear()) {
      a_coord <- a_coord$transform(data, panel_scales)
      return(segmentsGrob(a_coord$x, a_coord$y, a_coord$xend, a_coord$yend,
        default.units = "native",
        gp = gpar(
          col = alpha(a_coord$colour, a_coord$alpha),
          fill = alpha(a_coord$colour, a_coord$alpha),
          lwd = a_coord$size * .pt,
          lty = a_coord$linetype,
          lineend = lineend
        ),
        arrow = arrow
      ))
    }

    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- plyr::rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
      warn_missing = FALSE)

    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group),]

    a_GeomPath$draw_panel(pieces, panel_scales, a_coord, arrow = arrow,
      lineend = lineend)
  },

  draw_key = a_draw_key_path
)
