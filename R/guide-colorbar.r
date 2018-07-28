#' Continuous colour bar guide.
#'
#' Colour bar guide shows continuous color scales mapped onto values.
#' Colour bar is available with \code{a_scale_fill} and \code{a_scale_colour}.
#' For more information, see the inspiration for this function:
#' \href{http://www.mathworks.com/help/techdoc/ref/colorbar.html}{Matlab's colorbar function}.
#'
#' Guides can be specified in each \code{a_scale_*} or in \code{\link{a_guides}}.
#' \code{a_guide="legend"} in \code{a_scale_*} is syntactic sugar for
#' \code{a_guide=a_guide_legend()} (e.g. \code{a_scale_color_manual(a_guide = "legend")}).
#' As for how to specify the a_guide for each scale in more detail,
#' see \code{\link{a_guides}}.
#'
#' @inheritParams a_guide_legend
#' @param barwidth A numeric or a \code{\link[grid]{unit}} object specifying
#'   the width of the colorbar. Default value is \code{legend.key.width} or
#'   \code{legend.key.size} in \code{\link{a_theme}} or a_theme.
#' @param barheight A numeric or a \code{\link[grid]{unit}} object specifying
#'   the height of the colorbar. Default value is \code{legend.key.height} or
#'   \code{legend.key.size} in \code{\link{a_theme}} or a_theme.
#' @param nbin A numeric specifying the number of bins for drawing colorbar. A
#'   smoother colorbar for a larger value.
#' @param raster A logical. If \code{TRUE} then the colorbar is rendered as a
#'   raster object. If \code{FALSE} then the colorbar is rendered as a set of
#'   rectangles. Note that not all graphics devices are capable of rendering
#'   raster image.
#' @param ticks A logical specifying if tick marks on colorbar should be
#'   visible.
#' @param draw.ulim A logical specifying if the upper limit tick marks should
#'   be visible.
#' @param draw.llim A logical specifying if the lower limit tick marks should
#'   be visible.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating \code{\link[grid]{unit}}
#'   for \code{barwidth} and \code{barheight}.
#' @param reverse logical. If \code{TRUE} the colorbar is reversed. By default,
#'   the highest value is on the top and the lowest value is on the bottom
#' @param ... ignored.
#' @return A guide object
#' @export
#' @family a_guides
#' @examples
#' df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#'
#' p1 <- a_plot(df, aes(X1, X2)) + a_geom_tile(aes(fill = value))
#' p2 <- p1 + a_geom_point(aes(size = value))
#'
#' # Basic form
#' p1 + a_scale_fill_continuous(a_guide = "colorbar")
#' p1 + a_scale_fill_continuous(a_guide = a_guide_colorbar())
#' p1 + a_guides(fill = a_guide_colorbar())
#'
#' # Control styles
#'
#' # bar size
#' p1 + a_guides(fill = a_guide_colorbar(barwidth = 0.5, barheight = 10))
#'
#' # no label
#' p1 + a_guides(fill = a_guide_colorbar(label = FALSE))
#'
#' # no tick marks
#' p1 + a_guides(fill = a_guide_colorbar(ticks = FALSE))
#'
#' # label position
#' p1 + a_guides(fill = a_guide_colorbar(label.a_position = "left"))
#'
#' # label a_theme
#' p1 + a_guides(fill = a_guide_colorbar(label.a_theme = a_element_text(colour = "blue", angle = 0)))
#'
#' # small number of bins
#' p1 + a_guides(fill = a_guide_colorbar(nbin = 3))
#'
#' # large number of bins
#' p1 + a_guides(fill = a_guide_colorbar(nbin = 100))
#'
#' # make top- and bottom-most ticks invisible
#' p1 + a_scale_fill_continuous(limits = c(0,20), breaks = c(0, 5, 10, 15, 20),
#'  a_guide = a_guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))
#'
#' # guides can be controlled independently
#' p2 +
#'   a_scale_fill_continuous(a_guide = "colorbar") +
#'   a_scale_size(a_guide = "legend")
#' p2 + a_guides(fill = "colorbar", size = "legend")
#'
#' p2 +
#'   a_scale_fill_continuous(a_guide = a_guide_colorbar(direction = "horizontal")) +
#'   a_scale_size(a_guide = a_guide_legend(direction = "vertical"))
a_guide_colourbar <- function(

  # title
  title = waiver(),
  title.a_position = NULL,
  title.a_theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # label
  label = TRUE,
  label.a_position = NULL,
  label.a_theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 20,
  raster = TRUE,

  # ticks
  ticks = TRUE,
  draw.ulim= TRUE,
  draw.llim = TRUE,

  # general
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,

  ...) {

  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list(
    # title
    title = title,
    title.a_position = title.a_position,
    title.a_theme = title.a_theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.a_position = label.a_position,
    label.a_theme = label.a_theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # bar
    barwidth = barwidth,
    barheight = barheight,
    nbin = nbin,
    raster = raster,

    # ticks
    ticks = ticks,
    draw.ulim = draw.ulim,
    draw.llim = draw.llim,

    # general
    direction = direction,
    default.unit = default.unit,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = c("colour", "color", "fill"), ..., name = "colorbar"),
    class = c("a_guide", "colorbar")
  )
}

#' @export
a_guide_train.colorbar <- function(a_guide, a_scale) {

  # do nothing if a_scale are inappropriate
  if (length(intersect(a_scale$aesthetics, c("color", "colour", "fill"))) == 0) {
    warning("colorbar guide needs colour or fill scales.")
    return(NULL)
  }
  if (a_scale$is_discrete()) {
    warning("colorbar guide needs continuous scales.")
    return(NULL)
  }


  # create data frame for tick display
  breaks <- a_scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()

  ticks <- as.data.frame(setNames(list(a_scale$map(breaks)), a_scale$aesthetics[1]))
  ticks$.value <- breaks
  ticks$.label <- a_scale$get_labels(breaks)

  a_guide$key <- ticks

  # bar specification (number of divs etc)
  .limits <- a_scale$get_limits()
  .bar <- discard(pretty(.limits, n = a_guide$nbin), a_scale$get_limits())
  if (length(.bar) == 0) {
    .bar = unique(.limits)
  }
  a_guide$bar <- data.frame(colour = a_scale$map(.bar), value = .bar, stringsAsFactors = FALSE)
  if (a_guide$reverse) {
    a_guide$key <- a_guide$key[nrow(a_guide$key):1, ]
    a_guide$bar <- a_guide$bar[nrow(a_guide$bar):1, ]
  }
  a_guide$hash <- with(a_guide, digest::digest(list(title, key$.label, bar, name)))
  a_guide
}

# simply discards the new a_guide
#' @export
a_guide_merge.colorbar <- function(a_guide, new_guide) {
  a_guide
}

# this guide is not geom-based.
#' @export
a_guide_geom.colorbar <- function(a_guide, ...) {
  a_guide
}

#' @export
a_guide_gengrob.colorbar <- function(a_guide, a_theme) {

  # settings of location and size
  switch(a_guide$direction,
    "horizontal" = {
      label.a_position <- a_guide$label.a_position %||% "bottom"
      if (!label.a_position %in% c("top", "bottom")) stop("label a_position \"", label.a_position, "\" is invalid")

      barwidth <- convertWidth(a_guide$barwidth %||% (a_theme$legend.key.width * 5), "mm")
      barheight <- convertHeight(a_guide$barheight %||% a_theme$legend.key.height, "mm")
    },
    "vertical" = {
      label.a_position <- a_guide$label.a_position %||% "right"
      if (!label.a_position %in% c("left", "right")) stop("label a_position \"", label.a_position, "\" is invalid")

      barwidth <- convertWidth(a_guide$barwidth %||% a_theme$legend.key.width, "mm")
      barheight <- convertHeight(a_guide$barheight %||% (a_theme$legend.key.height * 5), "mm")
    })

  barwidth.c <- c(barwidth)
  barheight.c <- c(barheight)
  barlength.c <- switch(a_guide$direction, "horizontal" = barwidth.c, "vertical" = barheight.c)
  nbreak <- nrow(a_guide$key)

  # gap between keys etc
  hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  grob.bar <-
    if (a_guide$raster) {
      image <- switch(a_guide$direction, horizontal = t(a_guide$bar$colour), vertical = rev(a_guide$bar$colour))
      rasterGrob(image = image, width = barwidth.c, height = barheight.c, default.units = "mm", gp = gpar(col = NA), interpolate = TRUE)
    } else {
      switch(a_guide$direction,
             horizontal = {
               bw <- barwidth.c / nrow(a_guide$bar)
               bx <- (seq(nrow(a_guide$bar)) - 1) * bw
               rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight.c, default.units = "mm",
                        gp = gpar(col = NA, fill = a_guide$bar$colour))
             },
             vertical = {
               bh <- barheight.c / nrow(a_guide$bar)
               by <- (seq(nrow(a_guide$bar)) - 1) * bh
               rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth.c, height = bh, default.units = "mm",
                        gp = gpar(col = NA, fill = a_guide$bar$colour))
             })
  }

  # tick and label position
  tic_pos.c <- rescale(a_guide$key$.value, c(0.5, a_guide$nbin - 0.5), a_guide$bar$value[c(1, nrow(a_guide$bar))]) * barlength.c / a_guide$nbin
  label_pos <- unit(tic_pos.c, "mm")
  if (!a_guide$draw.ulim) tic_pos.c <- tic_pos.c[-1]
  if (!a_guide$draw.llim) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]

  # title
  grob.title <- ggname("a_guide.title",
    a_element_grob(
      a_guide$title.a_theme %||% calc_element("legend.title", a_theme),
      label = a_guide$title,
      hjust = a_guide$title.hjust %||% a_theme$legend.title.align %||% 0,
      vjust = a_guide$title.vjust %||% 0.5
    )
  )


  title_width <- convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  # label
  label.a_theme <- a_guide$label.a_theme %||% calc_element("legend.text", a_theme)
  grob.label <- {
    if (!a_guide$label)
      a_zeroGrob()
    else {
      hjust <- x <- a_guide$label.hjust %||% a_theme$legend.text.align %||%
        if (any(is.expression(a_guide$key$.label))) 1 else switch(a_guide$direction, horizontal = 0.5, vertical = 0)
      vjust <- y <- a_guide$label.vjust %||% 0.5
      switch(a_guide$direction, horizontal = {x <- label_pos; y <- vjust}, "vertical" = {x <- hjust; y <- label_pos})

      label <- a_guide$key$.label

      # If any of the labels are quoted language objects, convert them
      # to expressions. Labels from formatter functions can return these
      if (any(vapply(label, is.call, logical(1)))) {
        label <- lapply(label, function(l) {
          if (is.call(l)) substitute(expression(x), list(x = l))
          else l
        })
        label <- do.call(c, label)
      }
      g <- a_element_grob(a_element = label.a_theme, label = label,
        x = x, y = y, hjust = hjust, vjust = vjust)
      ggname("a_guide.label", g)
    }
  }

  label_width <- convertWidth(grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- convertHeight(grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)

  # ticks
  grob.ticks <-
    if (!a_guide$ticks) a_zeroGrob()
    else {
      switch(a_guide$direction,
        "horizontal" = {
          x0 = rep(tic_pos.c, 2)
          y0 = c(rep(0, nbreak), rep(barheight.c * (4/5), nbreak))
          x1 = rep(tic_pos.c, 2)
          y1 = c(rep(barheight.c * (1/5), nbreak), rep(barheight.c, nbreak))
        },
        "vertical" = {
          x0 = c(rep(0, nbreak), rep(barwidth.c * (4/5), nbreak))
          y0 = rep(tic_pos.c, 2)
          x1 = c(rep(barwidth.c * (1/5), nbreak), rep(barwidth.c, nbreak))
          y1 = rep(tic_pos.c, 2)
        })
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                   default.units = "mm", gp = gpar(col = "white", lwd = 0.5, lineend = "butt"))
    }

  # layout of bar and label
  switch(a_guide$direction,
    "horizontal" = {
      switch(label.a_position,
        "top" = {
          bl_widths <- barwidth.c
          bl_heights <- c(label_height.c, vgap, barheight.c)
          vps <- list(bar.row = 3, bar.col = 1,
                      label.row = 1, label.col = 1)
        },
        "bottom" = {
          bl_widths <- barwidth.c
          bl_heights <- c(barheight.c, vgap, label_height.c)
          vps <- list(bar.row = 1, bar.col = 1,
                      label.row = 3, label.col = 1)
        })
    },
    "vertical" = {
      switch(label.a_position,
        "left" = {
          bl_widths <- c(label_width.c, vgap, barwidth.c)
          bl_heights <- barheight.c
          vps <- list(bar.row = 1, bar.col = 3,
                      label.row = 1, label.col = 1)
        },
        "right" = {
          bl_widths <- c(barwidth.c, vgap, label_width.c)
          bl_heights <- barheight.c
          vps <- list(bar.row = 1, bar.col = 1,
                      label.row = 1, label.col = 3)
        })
    })

  # layout of title and bar+label
  switch(a_guide$title.a_position,
    "top" = {
      widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
      heights <- c(title_height.c, vgap, bl_heights)
      vps <- with(vps,
                  list(bar.row = bar.row + 2, bar.col = bar.col,
                       label.row = label.row + 2, label.col = label.col,
                       title.row = 1, title.col = 1:length(widths)))
    },
    "bottom" = {
      widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
      heights <- c(bl_heights, vgap, title_height.c)
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col,
                       label.row = label.row, label.col = label.col,
                       title.row = length(heights), title.col = 1:length(widths)))
    },
    "left" = {
      widths <- c(title_width.c, hgap, bl_widths)
      heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col + 2,
                       label.row = label.row, label.col = label.col + 2,
                       title.row = 1:length(heights), title.col = 1))
    },
    "right" = {
      widths <- c(bl_widths, hgap, title_width.c)
      heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col,
                       label.row = label.row, label.col = label.col,
                       title.row = 1:length(heights), title.col = length(widths)))
    })

  # background
  grob.background <- a_element_render(a_theme, "legend.background")

  # padding
  padding <- unit(1.5, "mm")
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)

  gt <- gtable(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.bar, name = "bar", clip = "off",
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
    b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
  gt <- gtable_add_grob(gt, grob.label, name = "label", clip = "off",
    t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
    b = 1 + max(vps$label.row), l = 1 + min(vps$label.col))
  gt <- gtable_add_grob(gt, grob.title, name = "title", clip = "off",
    t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
    b = 1 + max(vps$title.row), l = 1 + min(vps$title.col))
  gt <- gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
    b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))

  gt
}

#' @export
#' @rdname a_guide_colourbar
a_guide_colorbar <- a_guide_colourbar
