#' @include geom-.r
NULL

#' Annotation: Custom grob.
#'
#' This is a special geom intended for use as static annotations
#' that are the same in every panel. These annotations will not
#' affect scales (i.e. the x and y axes will not grow to cover the range
#' of the grob, and the grob will not be modified by any ggplot settings or mappings).
#'
#' Most useful for adding tables, inset plots, and other grid-based decorations.
#'
#' @param grob grob to display
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @export
#' @note \code{a_annotation_custom} expects the grob to fill the entire viewport
#' defined by xmin, xmax, ymin, ymax. Grobs with a different (absolute) size
#' will be center-justified in that region.
#' Inf values can be used to fill the full plot panel (see examples).
#' @examples
#' # Dummy plot
#' df <- data.frame(x = 1:10, y = 1:10)
#' base <- a_plot(df, a_aes(x, y)) +
#'   a_geom_blank() +
#'   a_theme_bw()
#'
#' # Full panel a_annotation
#' base + a_annotation_custom(
#'   grob = grid::roundrectGrob(),
#'   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
#' )
#'
#' # Inset plot
#' df2 <- data.frame(x = 1 , y = 1)
#' g <- ggplotGrob(a_plot(df2, a_aes(x, y)) +
#'   a_geom_point() +
#'   a_theme(plot.background = a_element_rect(colour = "black")))
#' base + a_annotation_custom(grob = g, xmin = 1, xmax = 10, ymin = 8, ymax = 10)
a_annotation_custom <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) {
  a_layer(
    data = NULL,
    a_stat = a_StatIdentity,
    a_position = a_PositionIdentity,
    a_geom = a_GeomCustomAnn,
    inherit.a_aes = TRUE,
    params = list(
      grob = grob,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomCustomAnn <- a_ggproto("a_GeomCustomAnn", a_Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_scales, a_coord, grob, xmin, xmax,
                        ymin, ymax) {
    if (!inherits(a_coord, "a_CoordCartesian")) {
      stop("a_annotation_custom only works with Cartesian coordinates",
        call. = FALSE)
    }
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- a_coord$transform(corners, panel_scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    vp <- viewport(x = mean(x_rng), y = mean(y_rng),
                   width = diff(x_rng), height = diff(y_rng),
                   just = c("center","center"))
    editGrob(grob, vp = vp, name = paste(grob$name, a_annotation_id()))
  },

  default_aes = a_aes_(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
)

a_annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})
