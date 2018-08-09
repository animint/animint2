#' Wrap a 1d ribbon of panels into 2d.
#'
#' Most displays are roughly rectangular, so if you have a categorical
#' variable with many levels, it doesn't make sense to try and display them
#' all in one row (or one column). To solve this dilemma, \code{a_facet_wrap}
#' wraps a 1d sequence of panels into 2d, making best use of screen real estate.
#'
#' @param facets Either a formula or character vector. Use either a
#'   one sided formula, \code{~a + b}, or a character vector, \code{c("a", "b")}.
#' @param nrow,ncol Number of rows and columns.
#' @param scales should Scales be fixed (\code{"fixed"}, the default),
#'   free (\code{"free"}), or free in one dimension (\code{"free_x"},
#'   \code{"free_y"}).
#' @param switch By default, the labels are displayed on the top of
#'   the plot. If \code{switch} is \code{"x"}, they will be displayed
#'   to the bottom. If \code{"y"}, they will be displayed to the
#'   left, near the y axis.
#' @param dir Direction: either "h" for horizontal, the default, or "v", for
#'   vertical.
#' @inheritParams a_facet_grid
#' @export
#' @examples
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_facet_wrap(~class)
#'
#' # Control the number of rows and columns with nrow and ncol
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_facet_wrap(~class, nrow = 4)
#'
#' \donttest{
#' # You can facet by multiple variables
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_facet_wrap(~ cyl + drv)
#' # Or use a character vector:
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   facet_wrap(c("cyl", "drv"))
#'
#' # Use the `labeller` option to control how labels are printed:
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   facet_wrap(c("cyl", "drv"), labeller = "a_label_both")
#'
#' # To change the order in which the panels appear, change the levels
#' # of the underlying factor.
#' mpg$class2 <- reorder(mpg$class, mpg$displ)
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   facet_wrap(~class2)
#'
#' # By default, the same scales are used for all panels. You can allow
#' # scales to vary across the panels with the `scales` argument.
#' # Free scales make it easier to see patterns within each panel, but
#' # harder to compare across panels.
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   facet_wrap(~class, scales = "free")
#'
#' # To repeat the same data in every panel, simply construct a data frame
#' # that does not contain the facetting variable.
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point(data = transform(mpg, class = NULL), colour = "grey85") +
#'   a_geom_point() +
#'   facet_wrap(~class)
#'
#' # Use `switch` to display the facet labels near an axis, acting as
#' # a subtitle for this axis. This is typically used with free scales
#' # and a a_theme without boxes around strip labels.
#' a_plot(economics_long, a_aes(date, value)) +
#'   a_geom_line() +
#'   facet_wrap(~variable, scales = "free_y", nrow = 2, switch = "x") +
#'   a_theme(strip.background = a_element_blank())
#' }
a_facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "a_label_value", as.table = TRUE,
                       switch = NULL, drop = TRUE, dir = "h") {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  if (identical(dir, "v")) {
    # swap
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- sanitise_dim(nrow_swap)
    ncol <- sanitise_dim(ncol_swap)
  } else {
    nrow <- sanitise_dim(nrow)
    ncol <- sanitise_dim(ncol)
  }

  # Check for deprecated labellers
  labeller <- check_labeller(labeller)

  a_facet(
    facets = as.quoted(facets), free = free, shrink = shrink,
    as.table = as.table, switch = switch,
    drop = drop, ncol = ncol, nrow = nrow,
    labeller = labeller,
    dir = dir,
    subclass = "wrap"
  )
}

#' @export
a_facet_train_layout.wrap <- function(a_facet, data) {
  panels <- layout_wrap(data, a_facet$facets, a_facet$nrow, a_facet$ncol,
     a_facet$as.table, a_facet$drop, a_facet$dir)

  n <- nrow(panels)
  nrow <- max(panels$ROW)

  # Add scale identification
  panels$SCALE_X <- if (a_facet$free$x) seq_len(n) else 1L
  panels$SCALE_Y <- if (a_facet$free$y) seq_len(n) else 1L

  # Figure out where axes should go
  panels$AXIS_X <- if (a_facet$free$x) TRUE else panels$ROW == nrow
  panels$AXIS_Y <- if (a_facet$free$y) TRUE else panels$COL == 1

  panels
}

#' @export
a_facet_map_layout.wrap <- function(a_facet, data, layout) {
  locate_wrap(data, layout, a_facet$facets)
}

# How to think about facet wrap:
#  * every panel has strips (strip_pos) and axes (axis_pos)
#  * if scales fixed, most axes empty
#  * combine panels, strips and axes, then wrap into 2d
#  * finally: add title, labels and legend
#' @export
a_facet_render.wrap <- function(a_facet, panel, a_coord, a_theme, a_geom_grobs) {

  # If a_coord is (non-cartesian or flip) and (x is free or y is free)
  # then print a warning
  if ((!inherits(a_coord, "a_CoordCartesian") || inherits(a_coord, "a_CoordFlip")) &&
    (a_facet$free$x || a_facet$free$y)) {
    stop("ggplot2 does not currently support free scales with a non-cartesian coord or coord_flip.\n")
  }

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- a_theme$aspect.ratio
  if (is.null(aspect_ratio) && !a_facet$free$x && !a_facet$free$y) {
    aspect_ratio <- a_coord$aspect(panel$ranges[[1]])
  }

  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }

  layout <- panel$layout
  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)

  # Set switch to default value when misspecified
  switch_to_x <- FALSE
  switch_to_y <- FALSE
  if (!is.null(a_facet$switch) && a_facet$switch == "x") {
    switch_to_x <- TRUE
  } else if (!is.null(a_facet$switch) && a_facet$switch == "y") {
    switch_to_y <- TRUE
  } else if (!is.null(a_facet$switch)) {
    message("`switch` must be set to 'x', 'y' or NULL")
    a_facet$switch <- NULL
  }

  panels <- a_facet_panels(a_facet, panel, a_coord, a_theme, a_geom_grobs)
  axes <- a_facet_axes(a_facet, panel, a_coord, a_theme)
  strips <- a_facet_strips(a_facet, panel, a_theme)


  # Should become facet_arrange_grobs

  # Locate each element in panel
  find_pos <- function(layout, loc, size) {
    n <- nrow(layout)
    l <- size[1] * (layout$COL - 1) + loc[1]
    t <- size[2] * (layout$ROW - 1) + loc[2]
    data.frame(t = t, r = l, b = t, l = l, id = seq_len(n))
  }


  if (switch_to_x) {
    locs <- list(
      panel =   c(2, 1),
      strip_t = c(2, 3),
      axis_l =  c(1, 1),
      axis_b =  c(2, 2),
      hspace =  c(2, 4),
      vspace =  c(3, 1)
    )
  } else if (switch_to_y) {
    locs <- list(
      panel =   c(3, 1),
      strip_t = c(1, 1),
      axis_l =  c(2, 1),
      axis_b =  c(3, 2),
      hspace =  c(3, 3),
      vspace =  c(4, 1)
    )
  } else {
    locs <- list(
      panel =   c(2, 2),
      strip_t = c(2, 1),
      axis_l =  c(1, 2),
      axis_b =  c(2, 3),
      hspace =  c(2, 4),
      vspace =  c(3, 2)
    )
  }

  grobs <- list(
    panel = panels,
    strip_t = strips$t,
    axis_l = axes$l,
    axis_b = axes$b
  )

  # If strips are switched, add padding
  if (switch_to_x) {
    padding <- convertUnit(a_theme$strip.switch.pad.wrap, "cm")

    add_padding <- function(strip) {
      gt_t <- gtable_row("strip_t", list(strip),
        height = unit(height_cm(strip), "cm"))

      # One padding above and two below, so that the strip does not look
      # like a title for the panel just below.
      gt_t <- gtable_add_rows(gt_t, padding, pos = 0)
      gt_t <- gtable_add_rows(gt_t, 2 * padding, pos = 2)
      gt_t
    }
    grobs$strip_t <- lapply(strips$t, add_padding)

    strip_height <- lapply(strips$t, function(x) {
       3 * as.numeric(padding) + height_cm(x)
    })
    strip_width <- NULL
    size <- c(3, 4)

  } else if (switch_to_y) {
    padding <- convertUnit(a_theme$strip.switch.pad.wrap, "cm")

    add_padding <- function(strip) {
      gt_t <- gtable_col("strip_t", list(strip),
        heights = unit(aspect_ratio, "null"))

      gt_t <- gtable_add_cols(gt_t, padding, pos = 0)
      gt_t <- gtable_add_cols(gt_t, padding, pos = 2)
      gt_t
    }
    grobs$strip_t <- lapply(strips$t, add_padding)

    strip_height <- NULL
    strip_width <- lapply(strips$t, function(x) {
      3 * as.numeric(padding) + width_cm(x)
    })
    size <- c(4, 3)

  } else {
    strip_height <- height_cm(grobs$strip_t)
    strip_width <- NULL
    size <- c(3, 4)
  }

  info <- plyr::ldply(locs, find_pos, layout = layout, size = size)
  names(info)[1] <- "type"
  info$clip <- ifelse(info$type == "panel", "on", "off")
  info$name <- paste(info$type, info$id, sep = "-")

  # Bare numbers are taken as cm
  # If not listed, assume is unit(1, "null")
  widths <- list(
    axis_l = width_cm(grobs$axis_l),
    strip_t = strip_width,
    vspace = ifelse(layout$COL == ncol, 0, width_cm(a_theme$panel.margin.x %||% a_theme$panel.margin))
  )
  heights <- list(
    panel = unit(aspect_ratio, "null"),
    strip_t = strip_height,
    axis_b = height_cm(grobs$axis_b),
    hspace = ifelse(layout$ROW == nrow, 0, height_cm(a_theme$panel.margin.y %||% a_theme$panel.margin))
  )

  # Remove strip_t according to which strips are switched
  heights <- Filter(Negate(is.null), heights)
  widths <- Filter(Negate(is.null), widths)

  col_widths <- compute_grob_widths(info, widths)
  row_heights <- compute_grob_heights(info, heights)

  # Create the gtable for the legend
  gt <- gtable(widths = col_widths, heights = row_heights, respect = respect)

  # Keep only the rows in info that refer to grobs
  info  <- info[info$type %in% names(grobs), ]
  grobs <- unlist(grobs, recursive = FALSE)

  # Add the grobs
  gt <- gtable_add_grob(gt, grobs, l = info$l, t = info$t, r = info$r,
    b = info$b, name = info$name, clip = info$clip)

  gt
}

#' @export
a_facet_panels.wrap <- function(a_facet, panel, a_coord, a_theme, a_geom_grobs) {
  panels <- panel$layout$PANEL
  lapply(panels, function(i) {
    fg <- a_coord$render_fg(panel$ranges[[i]], a_theme)
    bg <- a_coord$render_bg(panel$ranges[[i]], a_theme)

    a_geom_grobs <- lapply(a_geom_grobs, "[[", i)

    if (a_theme$panel.ontop) {
      panel_grobs <- c(a_geom_grobs, list(bg), list(fg))
    } else {
      panel_grobs <- c(list(bg), a_geom_grobs, list(fg))
    }

    ggname(paste("panel", i, sep = "-"),
      gTree(children = do.call("gList", panel_grobs)))
  })
}

#' @export
a_facet_strips.wrap <- function(a_facet, panel, a_theme) {
  a_labels_df <- panel$layout[names(a_facet$facets)]

  # Adding a_labels metadata, useful for labellers
  attr(a_labels_df, "a_facet") <- "wrap"
  if (is.null(a_facet$switch) || a_facet$switch == "x") {
    dir <- "b"
    attr(a_labels_df, "type") <- "rows"
  } else {
    dir <- "l"
    attr(a_labels_df, "type") <- "cols"
  }

  strips_table <- a_build_strip(panel, a_labels_df, a_facet$labeller,
    a_theme, dir, switch = a_facet$switch)

  # While grid a_facetting works with a whole gtable, wrap processes the
  # strips separately. So we turn the gtable into a list
  if (dir == "b") {
    n_strips <- ncol(strips_table)
  }  else {
    n_strips <- nrow(strips_table)
  }

  strips <- list(t = vector("list", n_strips))
  for (i in seq_along(strips$t)) {
    if (dir == "b") {
      strips$t[[i]] <- strips_table[, i]
    } else {
      strips$t[[i]] <- strips_table[i, ]
    }
  }
  strips
}


#' @export
a_facet_axes.wrap <- function(a_facet, panel, a_coord, a_theme) {
  panels <- panel$layout$PANEL

  axes <- list()
  axes$b <- lapply(panels, function(i) {
    if (panel$layout$AXIS_X[i]) {
      grob <- a_coord$render_axis_h(panel$ranges[[i]], a_theme)
    } else {
      grob <- a_zeroGrob()
    }
    ggname(paste("axis-b-", i, sep = ""), grob)
  })

  axes$l <- lapply(panels, function(i) {
    if (panel$layout$AXIS_Y[i]) {
      grob <- a_coord$render_axis_v(panel$ranges[[i]], a_theme)
    } else {
      grob <- a_zeroGrob()
    }
    ggname(paste("axis-l-", i, sep = ""), grob)
  })
  axes

}

#' @export
a_facet_vars.wrap <- function(a_facet) {
  paste(lapply(a_facet$facets, paste, collapse = ", "), collapse = " ~ ")
}

#' Sanitise the number of rows or columns
#'
#' Cleans up the input to be an integer greater than or equal to one, or
#' \code{NULL}. Intended to be used on the \code{nrow} and \code{ncol}
#' arguments of \code{a_facet_wrap}.
#' @param n Hopefully an integer greater than or equal to one, or \code{NULL},
#' though other inputs are handled.
#' @return An integer greater than or equal to one, or \code{NULL}.
#' @note If the length of the input is greater than one, only the first element
#' is returned, with a warning.
#' If the input is not an integer, it will be coerced to be one.
#' If the value is less than one, \code{NULL} is returned, effectively ignoring
#' the argument.
#' Multiple warnings may be generated.
#' @examples
#' # Valid input just gets returns unchanged
#' sanitise_dim(1)
#' sanitise_dim(NULL)
#'
#' # Only the first element of vectors get returned
#' sanitise_dim(10:1)
#' # Non-integer values are coerced to integer
#' sanitise_dim(pi)
#' # Missing values, values less than one and non-numeric values are
#' # treated as NULL
#' sanitise_dim(NA_integer_)
#' sanitise_dim(0)
#' sanitise_dim("foo")
#' @noRd
sanitise_dim <- function(n) {
  xname <- paste0("`", deparse(substitute(n)), "`")
  if (length(n) == 0) {
    if (!is.null(n)) {
      warning(xname, " has length zero and will be treated as NULL.",
        call. = FALSE)
    }
    return(NULL)
  }
  if (length(n) > 1) {
    warning("Only the first value of ", xname, " will be used.", call. = FALSE)
    n <- n[1]
  }
  if (!is.numeric(n) || (!is.na(n) && n != round(n))) {
    warning("Coercing ", xname, " to be an integer.", call. = FALSE)
    n <- as.integer(n)
  }
  if (is.na(n) || n < 1) {
    warning(xname, " is missing or less than 1 and will be treated as NULL.",
      call. = FALSE)
    return(NULL)
  }
  n
}

