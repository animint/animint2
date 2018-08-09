#' Vertical intervals: lines, crossbars & errorbars.
#'
#' Various ways of representing a vertical interval defined by \code{x},
#' \code{ymin} and \code{ymax}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "linerange")}
#'
#' @param fatten A multiplicative factor used to increase the size of the
#'   middle bar in \code{a_geom_crossbar()} and the middle point in
#'   \code{a_geom_pointrange()}.
#' @seealso
#'  \code{\link{a_stat_summary}} for examples of these guys in use,
#'  \code{\link{a_geom_smooth}} for continuous analog
#' @export
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @examples
#' #' # Create a simple example dataset
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#'
#' p <- a_plot(df, a_aes(trt, resp, colour = group))
#' p + a_geom_linerange(a_aes(ymin = lower, ymax = upper))
#' p + a_geom_pointrange(a_aes(ymin = lower, ymax = upper))
#' p + a_geom_crossbar(a_aes(ymin = lower, ymax = upper), width = 0.2)
#' p + a_geom_errorbar(a_aes(ymin = lower, ymax = upper), width = 0.2)
#'
#' # Draw lines connecting group means
#' p +
#'   a_geom_line(a_aes(group = group)) +
#'   a_geom_errorbar(a_aes(ymin = lower, ymax = upper), width = 0.2)
#'
#' # If you want to dodge bars and errorbars, you need to manually
#' # specify the dodge width
#' p <- a_plot(df, a_aes(trt, resp, fill = group))
#' p +
#'  a_geom_bar(a_position = "dodge", a_stat = "identity") +
#'  a_geom_errorbar(a_aes(ymin = lower, ymax = upper), a_position = "dodge", width = 0.25)
#'
#' # Because the bars and errorbars have different widths
#' # we need to specify how wide the objects we are dodging are
#' dodge <- a_position_dodge(width=0.9)
#' p +
#'   a_geom_bar(a_position = dodge, a_stat = "identity") +
#'   a_geom_errorbar(a_aes(ymin = lower, ymax = upper), a_position = dodge, width = 0.25)
a_geom_linerange <- function(mapping = NULL, data = NULL,
                           a_stat = "identity", a_position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomLinerange,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomLinerange <- a_ggproto("a_GeomLinerange", a_Geom,
  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_key = a_draw_key_vpath,

  required_aes = c("x", "ymin", "ymax"),

  draw_panel = function(data, panel_scales, a_coord) {
    data <- transform(data, xend = x, y = ymin, yend = ymax)
    ggname("geom_linerange", a_GeomSegment$draw_panel(data, panel_scales, a_coord))
  }
)
