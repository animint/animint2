#' Legend guide.
#'
#' Legend type guide shows key (i.e., geoms) mapped onto values.
#' Legend guides for various scales are integrated if possible.
#'
#' Guides can be specified in each \code{a_scale_*} or in \code{\link{a_guides}}.
#' \code{a_guide="legend"} in \code{a_scale_*} is syntactic sugar for
#' \code{a_guide=a_guide_legend()} (e.g. \code{a_scale_color_manual(a_guide = "legend")}).
#' As for how to specify the a_guide for each scale in more detail,
#' see \code{\link{a_guides}}.
#'
#' @param title A character string or expression indicating a title of guide.
#'   If \code{NULL}, the title is not shown. By default
#'   (\code{\link{waiver}}), the name of the scale object or the name
#'   specified in \code{\link{labs}} is used for the title.
#' @param title.a_position A character string indicating the a_position of a
#'   title. One of "top" (default for a vertical a_guide), "bottom", "left"
#'  (default for a horizontal a_guide), or "right."
#' @param title.a_theme A a_theme object for rendering the title text. Usually the
#'   object of \code{\link{a_element_text}} is expected. By default, the a_theme is
#'   specified by \code{legend.title} in \code{\link{a_theme}} or a_theme.
#' @param title.hjust A number specifying horizontal justification of the
#'   title text.
#' @param title.vjust A number specifying vertical justification of the title
#'   text.
#' @param a_label logical. If \code{TRUE} then the labels are drawn. If
#'   \code{FALSE} then the labels are invisible.
#' @param a_label.a_position A character string indicating the a_position of a
#'   a_label. One of "top", "bottom" (default for horizontal guide), "left", or
#'   "right" (default for vertical guide).
#' @param a_label.a_theme A a_theme object for rendering the a_label text. Usually the
#'   object of \code{\link{a_element_text}} is expected. By default, the a_theme is
#'   specified by \code{legend.text} in \code{\link{a_theme}} or a_theme.
#' @param a_label.hjust A numeric specifying horizontal justification of the
#'   a_label text.
#' @param a_label.vjust A numeric specifying vertical justification of the a_label
#'   text.
#' @param keywidth A numeric or a \code{\link[grid]{unit}} object specifying
#'   the width of the legend key. Default value is \code{legend.key.width} or
#'   \code{legend.key.size} in \code{\link{a_theme}} or a_theme.
#' @param keyheight A numeric or a \code{\link[grid]{unit}} object specifying
#'   the height of the legend key. Default value is \code{legend.key.height} or
#'   \code{legend.key.size} in \code{\link{a_theme}} or a_theme.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating \code{\link[grid]{unit}}
#'   for \code{keywidth} and \code{keyheight}.
#' @param override.a_aes A list specifying aesthetic parameters of legend key.
#'   See details and examples.
#' @param nrow The desired number of rows of legends.
#' @param ncol The desired number of column of legends.
#' @param byrow logical. If \code{FALSE} (the default) the legend-matrix is
#'   filled by columns, otherwise the legend-matrix is filled by rows.
#' @param reverse logical. If \code{TRUE} the order of legends is reversed.
#' @param order positive integer less that 99 that specifies the order of
#'   this guide among multiple guides. This controls the order in which
#'   multiple guides are displayed, not the contents of the guide itself.
#'   If 0 (default), the order is determined by a secret algorithm.
#' @param ... ignored.
#' @return A guide object
#' @export
#' @family a_guides
#' @examples
#' \donttest{
#' df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#'
#' p1 <- a_plot(df, a_aes(X1, X2)) + a_geom_tile(a_aes(fill = value))
#' p2 <- p1 + a_geom_point(a_aes(size = value))
#'
#' # Basic form
#' p1 + a_scale_fill_continuous(a_guide = "legend")
#' p1 + a_scale_fill_continuous(a_guide = a_guide_legend())
#'
#' # Guide title
#' p1 + a_scale_fill_continuous(a_guide = a_guide_legend(title = "V")) # title text
#' p1 + a_scale_fill_continuous(a_guide = a_guide_legend(title = NULL)) # no title
#'
#' # Control styles
#'
#' # key size
#' p1 + a_guides(fill = a_guide_legend(keywidth = 3, keyheight = 1))
#'
#' # title position
#' p1 + a_guides(fill = a_guide_legend(title = "LEFT", title.a_position = "left"))
#'
#' # title text styles via a_element_text
#' p1 + a_guides(fill =
#'   a_guide_legend(
#'     title.a_theme = a_element_text(
#'       size = 15,
#'       face = "italic",
#'       colour = "red",
#'       angle = 0
#'     )
#'   )
#' )
#'
#' # a_label position
#' p1 + a_guides(fill = a_guide_legend(a_label.a_position = "left", a_label.hjust = 1))
#'
#' # a_label styles
#' p1 + a_scale_fill_continuous(breaks = c(5, 10, 15),
#'   a_labels = paste("long", c(5, 10, 15)),
#'   a_guide = a_guide_legend(
#'     direction = "horizontal",
#'     title.a_position = "top",
#'     a_label.a_position = "bottom",
#'     a_label.hjust = 0.5,
#'     a_label.vjust = 1,
#'     a_label.a_theme = a_element_text(angle = 90)
#'   )
#' )
#'
#' # Set aesthetic of legend key
#'
#' # very low alpha value make it difficult to see legend key
#' p3 <- a_plot(diamonds, a_aes(carat, price)) +
#'   a_geom_point(a_aes(colour = color), alpha = 1/100)
#' p3
#'
#' # override.a_aes overwrites the alpha
#' p3 + a_guides(colour = a_guide_legend(override.a_aes = list(alpha = 1)))
#'
#' # multiple row/col legends
#' df <- data.frame(x = 1:20, y = 1:20, color = letters[1:20])
#' p <- a_plot(df, a_aes(x, y)) +
#'   a_geom_point(a_aes(colour = color))
#' p + a_guides(col = a_guide_legend(nrow = 8))
#' p + a_guides(col = a_guide_legend(ncol = 8))
#' p + a_guides(col = a_guide_legend(nrow = 8, byrow = TRUE))
#' p + a_guides(col = a_guide_legend(ncol = 8, byrow = TRUE))
#'
#' # reversed order legend
#' p + a_guides(col = a_guide_legend(reverse = TRUE))
#' }
a_guide_legend <- function(

  # title
  title = waiver(),
  title.a_position = NULL,
  title.a_theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # a_label
  a_label = TRUE,
  a_label.a_position = NULL,
  a_label.a_theme = NULL,
  a_label.hjust = NULL,
  a_label.vjust = NULL,

  # key
  keywidth = NULL,
  keyheight = NULL,

  # general
  direction = NULL,
  default.unit = "line",
  override.a_aes = list(),
  nrow = NULL,
  ncol = NULL,
  byrow = FALSE,
  reverse = FALSE,
  order = 0,

  ...) {

  if (!is.null(keywidth) && !is.unit(keywidth)) keywidth <- unit(keywidth, default.unit)
  if (!is.null(keyheight) && !is.unit(keyheight)) keyheight <- unit(keyheight, default.unit)

  structure(
    list(
      # title
      title = title,
      title.a_position = title.a_position,
      title.a_theme = title.a_theme,
      title.hjust = title.hjust,
      title.vjust = title.vjust,

      # a_label
      a_label = a_label,
      a_label.a_position = a_label.a_position,
      a_label.a_theme = a_label.a_theme,
      a_label.hjust = a_label.hjust,
      a_label.vjust = a_label.vjust,

      # size of key
      keywidth = keywidth,
      keyheight = keyheight,

      # general
      direction = direction,
      override.a_aes = rename_aes(override.a_aes),
      nrow = nrow,
      ncol = ncol,
      byrow = byrow,
      reverse = reverse,
      order = order,

      # parameter
      available_aes = c("any"),
      ...,
      name = "legend"
    ),
    class = c("a_guide", "legend")
  )
}

#' @export
a_guide_train.legend <- function(a_guide, a_scale) {
  breaks <- a_scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()

  key <- as.data.frame(setNames(list(a_scale$map(breaks)), a_scale$a_aesthetics[1]),
    stringsAsFactors = FALSE)
  key$.a_label <- a_scale$get_labels(breaks)

  # this is a quick fix for #118
  # some scales have NA as na.value (e.g., size)
  # some scales have non NA as na.value (e.g., "grey50" for colour)
  # drop rows if data (instead of the mapped value) is NA
  #
  # Also, drop out-of-range values for continuous scale
  # (should use scale$oob?)
  if (a_scale$is_discrete()) {
    key <- key[!is.na(breaks), , drop = FALSE]
  } else {
    limits <- a_scale$get_limits()
    noob <- !is.na(breaks) & limits[1] <= breaks & breaks <= limits[2]
    key <- key[noob, , drop = FALSE]
  }


  if (a_guide$reverse) key <- key[nrow(key):1, ]

  a_guide$key <- key
  a_guide$hash <- with(a_guide, digest::digest(list(title, key$.a_label, direction, name)))
  a_guide
}

#' @export
a_guide_merge.legend <- function(a_guide, new_guide) {
  a_guide$key <- merge(a_guide$key, new_guide$key, sort = FALSE)
  a_guide$override.a_aes <- c(a_guide$override.a_aes, new_guide$override.a_aes)
  if (any(duplicated(names(a_guide$override.a_aes)))) warning("Duplicated override.a_aes is ignored.")
  a_guide$override.a_aes <- a_guide$override.a_aes[!duplicated(names(a_guide$override.a_aes))]
  a_guide
}

#' @export
a_guide_geom.legend <- function(a_guide, layers, default_mapping) {
  # arrange common data for vertical and horizontal guide
  a_guide$geoms <- plyr::llply(layers, function(a_layer) {
    all <- names(c(a_layer$mapping, if (a_layer$inherit.a_aes) default_mapping, a_layer$a_stat$default_aes))
    a_geom <- c(a_layer$a_geom$required_aes, names(a_layer$a_geom$default_aes))
    matched <- intersect(intersect(all, a_geom), names(a_guide$key))
    matched <- setdiff(matched, names(a_layer$a_geom_params))
    matched <- setdiff(matched, names(a_layer$a_aes_params))

    if (length(matched) > 0) {
      # This a_layer contributes to the legend
      if (is.na(a_layer$show.legend) || a_layer$show.legend) {
        # Default is to include it
        data <- a_layer$a_geom$use_defaults(a_guide$key[matched], a_layer$a_aes_params)
      } else {
        return(NULL)
      }
    } else {
      # This a_layer does not contribute to the legend
      if (is.na(a_layer$show.legend) || !a_layer$show.legend) {
        # Default is to exclude it
        return(NULL)
      } else {
        data <- a_layer$a_geom$use_defaults(NULL, a_layer$a_aes_params)[rep(1, nrow(a_guide$key)), ]
      }
    }

    # override.a_aes in a_guide_legend manually changes the a_geom
    data <- utils::modifyList(data, a_guide$override.a_aes)

    list(
      draw_key = a_layer$a_geom$draw_key,
      data = data,
      params = c(a_layer$a_geom_params, a_layer$a_stat_params)
    )
  })

  # remove null a_geom
  a_guide$geoms <- compact(a_guide$geoms)

  # Finally, remove this a_guide if no layer is drawn
  if (length(a_guide$geoms) == 0) a_guide <- NULL
  a_guide
}

#' @export
a_guide_gengrob.legend <- function(a_guide, a_theme) {

  # default setting
  a_label.a_position <- a_guide$a_label.a_position %||% "right"
  if (!a_label.a_position %in% c("top", "bottom", "left", "right"))
    stop("a_label a_position \"", a_label.a_position, "\" is invalid")

  nbreak <- nrow(a_guide$key)

  # gap between keys etc
  hgap <- width_cm(unit(0.3, "lines"))
  vgap <- hgap

  grob.title <- ggname("a_guide.title",
    a_element_grob(
      a_guide$title.a_theme %||% calc_element("legend.title", a_theme),
      a_label = a_guide$title,
      hjust = a_guide$title.hjust %||% a_theme$legend.title.align %||% 0,
      vjust = a_guide$title.vjust %||% 0.5,
      expand_x = FALSE,
      expand_y = FALSE
    )
  )

  title_width <- width_cm(grob.title)
  title_height <- height_cm(grob.title)

  # a_labels
  if (!a_guide$a_label || is.null(a_guide$key$.a_label)) {
    grob.a_labels <- rep(list(a_zeroGrob()), nrow(a_guide$key))
  } else {
    a_label.a_theme <- a_guide$a_label.a_theme %||% calc_element("legend.text", a_theme)

    # a_label.a_theme in param of a_guide_legend() > a_theme$legend.text.align > default
    # hjust/vjust in a_theme$legend.text and a_label.a_theme are ignored.
    hjust <- x <- a_guide$a_label.hjust %||% a_theme$legend.text.align %||%
      if (any(is.expression(a_guide$key$.a_label))) 1 else 0
    vjust <- y <- a_guide$a_label.vjust %||% 0.5

    grob.a_labels <- lapply(a_guide$key$.a_label, function(a_label, ...) {
      g <- a_element_grob(
        a_element = a_label.a_theme,
        a_label = a_label,
        x = x,
        y = y,
        hjust = hjust,
        vjust = vjust,
        expand_x = FALSE,
        expand_y = FALSE
      )
      ggname("a_guide.a_label", g)
    })
  }

  a_label_widths <- width_cm(grob.a_labels)
  a_label_heights <- height_cm(grob.a_labels)

  # Keys
  key_width <- width_cm(a_guide$keywidth %||% a_theme$legend.key.width %||% a_theme$legend.key.size)
  key_height <- height_cm(a_guide$keyheight %||% a_theme$legend.key.height %||% a_theme$legend.key.size)

  key_size_mat <- do.call("cbind", lapply(a_guide$geoms, function(g) g$data$size / 10))
  if (nrow(key_size_mat) == 0 || ncol(key_size_mat) == 0) {
    key_size_mat <- matrix(0, ncol = 1, nrow = nbreak)
  }
  key_sizes <- apply(key_size_mat, 1, max)

  if (!is.null(a_guide$nrow) && !is.null(a_guide$ncol) && a_guide$nrow * a_guide$ncol < nbreak)
    stop("`nrow` * `ncol` needs to be larger than the number of breaks", call. = FALSE)

  # If neither nrow/ncol specified, guess with "reasonable" values
  if (is.null(a_guide$nrow) && is.null(a_guide$ncol)) {
    if (a_guide$direction == "horizontal") {
      a_guide$nrow <- ceiling(nbreak / 5)
    } else {
      a_guide$ncol <- ceiling(nbreak / 20)
    }
  }
  legend.nrow <- a_guide$nrow %||% ceiling(nbreak / a_guide$ncol)
  legend.ncol <- a_guide$ncol %||% ceiling(nbreak / a_guide$nrow)

  key_sizes <- matrix(c(key_sizes, rep(0, legend.nrow * legend.ncol - nbreak)),
                      legend.nrow, legend.ncol, byrow = a_guide$byrow)

  key_widths <- pmax(key_width, apply(key_sizes, 2, max))
  key_heights <- pmax(key_height, apply(key_sizes, 1, max))

  a_label_widths <- apply(matrix(c(a_label_widths, rep(0, legend.nrow * legend.ncol - nbreak)),
                                 legend.nrow, legend.ncol, byrow = a_guide$byrow),
                          2, max)
  a_label_heights <- apply(matrix(c(a_label_heights, rep(0, legend.nrow * legend.ncol - nbreak)),
                                  legend.nrow, legend.ncol, byrow = a_guide$byrow),
                           1, max)

  if (a_guide$byrow) {
    vps <- data.frame(
      R = ceiling(seq(nbreak) / legend.ncol),
      C = (seq(nbreak) - 1) %% legend.ncol + 1
    )
  } else {
    vps <- as.data.frame(arrayInd(seq(nbreak), dim(key_sizes)))
    names(vps) <- c("R", "C")
  }

  # layout of key-a_label depends on the direction of the a_guide
  if (a_guide$byrow == TRUE) {
    switch(a_label.a_position,
      "top" = {
        kl_widths <- pmax(a_label_widths, key_widths)
        kl_heights <- utils::head(interleave(a_label_heights, vgap/2, key_heights, vgap/2), -1)
        vps <- transform(vps, key.row = R * 4 - 1, key.col = C, a_label.row = R * 4 - 3, a_label.col = C)
      },
      "bottom" = {
        kl_widths <- pmax(a_label_widths, key_widths)
        kl_heights <- utils::head(interleave(key_heights, vgap/2, a_label_heights, vgap/2), -1)
        vps <- transform(vps, key.row = R * 4 - 3, key.col = C, a_label.row = R * 4 - 1, a_label.col = C)
      },
      "left" = {
        kl_widths <- utils::head(interleave(a_label_widths, hgap/2, key_widths, hgap/2), -1)
        kl_heights <- utils::head(interleave(pmax(a_label_heights, key_heights), vgap/2), -1)
        vps <- transform(vps, key.row = R * 2 - 1, key.col = C * 4 - 1, a_label.row = R * 2 - 1, a_label.col = C * 4 - 3)
      },
      "right" = {
        kl_widths <- utils::head(interleave(key_widths, hgap/2, a_label_widths, hgap/2), -1)
        kl_heights <- utils::head(interleave(pmax(a_label_heights, key_heights), vgap/2), -1)
        vps <- transform(vps, key.row = R * 2 - 1, key.col = C * 4 - 3, a_label.row = R * 2 - 1, a_label.col = C * 4 - 1)
        })
  } else {
    switch(a_label.a_position,
      "top" = {
        kl_widths <- utils::head(interleave(pmax(a_label_widths, key_widths), hgap/2), -1)
        kl_heights <- utils::head(interleave(a_label_heights, vgap/2, key_heights, vgap/2), -1)
        vps <- transform(vps, key.row = R * 4 - 1, key.col = C * 2 - 1, a_label.row = R * 4 - 3, a_label.col = C * 2 - 1)
      },
      "bottom" = {
        kl_widths <- utils::head(interleave(pmax(a_label_widths, key_widths), hgap/2), -1)
        kl_heights <- utils::head(interleave(key_heights, vgap/2, a_label_heights, vgap/2), -1)
        vps <- transform(vps, key.row = R * 4 - 3, key.col = C * 2 - 1, a_label.row = R * 4 - 1, a_label.col = C * 2 - 1)
      },
      "left" = {
        kl_widths <- utils::head(interleave(a_label_widths, hgap/2, key_widths, hgap/2), -1)
        kl_heights <- pmax(key_heights, a_label_heights)
        vps <- transform(vps, key.row = R, key.col = C * 4 - 1, a_label.row = R, a_label.col = C * 4 - 3)
      },
      "right" = {
        kl_widths <- utils::head(interleave(key_widths, hgap/2, a_label_widths, hgap/2), -1)
        kl_heights <- pmax(key_heights, a_label_heights)
        vps <- transform(vps, key.row = R, key.col = C * 4 - 3, a_label.row = R, a_label.col = C * 4 - 1)
      })
  }

  # layout the title over key-a_label
  switch(a_guide$title.a_position,
    "top" = {
      widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
      heights <- c(title_height, vgap, kl_heights)
      vps <- transform(vps, key.row = key.row + 2, key.col = key.col, a_label.row = a_label.row + 2, a_label.col = a_label.col)
      vps.title.row = 1; vps.title.col = 1:length(widths)
    },
    "bottom" = {
      widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
      heights <- c(kl_heights, vgap, title_height)
      vps <- transform(vps, key.row = key.row, key.col = key.col, a_label.row = a_label.row, a_label.col = a_label.col)
      vps.title.row = length(heights); vps.title.col = 1:length(widths)
    },
    "left" = {
      widths <- c(title_width, hgap, kl_widths)
      heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
      vps <- transform(vps, key.row = key.row, key.col = key.col + 2, a_label.row = a_label.row, a_label.col = a_label.col + 2)
      vps.title.row = 1:length(heights); vps.title.col = 1
    },
    "right" = {
      widths <- c(kl_widths, hgap, title_width)
      heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
      vps <- transform(vps, key.row = key.row, key.col = key.col, a_label.row = a_label.row, a_label.col = a_label.col)
      vps.title.row = 1:length(heights); vps.title.col = length(widths)
    })

  # grob for key
  key_size <- c(key_width, key_height) * 10

  draw_key <- function(i) {
    bg <- a_element_render(a_theme, "legend.key")
    keys <- lapply(a_guide$geoms, function(g) {
      g$draw_key(g$data[i, ], g$params, key_size)
    })
    c(list(bg), keys)
  }
  grob.keys <- unlist(lapply(seq_len(nbreak), draw_key), recursive = FALSE)

  # background
  grob.background <- a_element_render(a_theme, "legend.background")

  ngeom <- length(a_guide$geoms) + 1
  kcols <- rep(vps$key.col, each = ngeom)
  krows <- rep(vps$key.row, each = ngeom)

  # padding
  padding <- 0.15
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)

  # Create the gtable for the legend
  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.title, name = "title", clip = "off",
    t = 1 + min(vps.title.row), r = 1 + max(vps.title.col),
    b = 1 + max(vps.title.row), l = 1 + min(vps.title.col))
  gt <- gtable_add_grob(gt, grob.keys,
    name = paste("key", krows, kcols, c("bg", seq(ngeom - 1)), sep = "-"), clip = "off",
    t = 1 + krows, r = 1 + kcols,
    b = 1 + krows, l = 1 + kcols)
  gt <- gtable_add_grob(gt, grob.a_labels,
    name = paste("a_label", vps$a_label.row, vps$a_label.col, sep = "-"), clip = "off",
    t = 1 + vps$a_label.row, r = 1 + vps$a_label.col,
    b = 1 + vps$a_label.row, l = 1 + vps$a_label.col)

  gt
}

globalVariables(c("C", "R", "key.row", "key.col", "a_label.row", "a_label.col"))
