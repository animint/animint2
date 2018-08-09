#' Facet specification: a single panel.
#'
#' @inheritParams a_facet_grid
#' @export
#' @examples
#' # facet_null is the default facetting specification if you
#' # don't override it with facet_grid or facet_wrap
#' a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point()
a_facet_null <- function(shrink = TRUE) {
  a_facet(shrink = shrink, subclass = "null")
}

#' @export
a_facet_train_layout.null <- function(a_facet, data) {
  data.frame(
    PANEL = 1L, ROW = 1L, COL = 1L,
    SCALE_X = 1L, SCALE_Y = 1L)
}

#' @export
a_facet_map_layout.null <- function(a_facet, data, layout) {
  # Need the is.waive check for special case where no data, but aesthetics
  # are mapped to vectors
  if (is.waive(data) || empty(data))
    return(cbind(data, PANEL = integer(0)))
  data$PANEL <- 1L
  data
}

#' @export
a_facet_render.null <- function(a_facet, panel, a_coord, a_theme, a_geom_grobs) {
  range <- panel$ranges[[1]]

  # Figure out aspect ratio
  aspect_ratio <- a_theme$aspect.ratio %||% a_coord$aspect(range)
  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }

  fg <- a_coord$render_fg(range, a_theme)
  bg <- a_coord$render_bg(range, a_theme)

  # Flatten layers - we know there's only one panel
  a_geom_grobs <- lapply(a_geom_grobs, "[[", 1)

  if (a_theme$panel.ontop) {
    panel_grobs <- c(a_geom_grobs, list(bg), list(fg))
  } else {
    panel_grobs <- c(list(bg), a_geom_grobs, list(fg))
  }

  panel_grob <- gTree(children = do.call("gList", panel_grobs))
  axis_h <- a_coord$render_axis_h(range, a_theme)
  axis_v <- a_coord$render_axis_v(range, a_theme)

  all <- matrix(list(
    axis_v,     panel_grob,
    a_zeroGrob(), axis_h
  ), ncol = 2, byrow = TRUE)

  layout <- gtable_matrix("layout", all,
    widths = unit.c(grobWidth(axis_v), unit(1, "null")),
    heights = unit.c(unit(aspect_ratio, "null"), grobHeight(axis_h)),
    respect = respect, clip = c("off", "off", "on", "off"),
    z = matrix(c(3, 2, 1, 4), ncol = 2, byrow = TRUE)
  )
  layout$layout$name <- c("axis-l", "spacer", "panel", "axis-b")

  layout
}

#' @export
a_facet_vars.null <- function(a_facet) ""
