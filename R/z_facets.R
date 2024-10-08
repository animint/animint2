# Determine titles to put on facet panels.
# The implementation here is a modified version of facet_strips.
getStrips <- function(facet, panel, ...)
  # ... is a placeholder at the moment in case we want to implement
  # themes or special options later
  UseMethod("getStrips")

#' @export
getStrips.grid <- function(facet, panel, ...) {
  npanels <- nrow(panel$layout)
  # preallocate strip labels to a default of ""
  strips.empty <- strips.right <- strips.top <- rep("", npanels)
  # right strips in a grid are only drawn on the last column
  row_vars <- unique(panel$layout[names(facet$rows)])
  strips.right[with(panel$layout, COL == max(COL))] <-
    build_strip(panel, row_vars, facet$labeller, side = "right", ...)
  # top strips in a grid layout are only drawn on the first row
  col_vars <- unique(panel$layout[names(facet$cols)])
  strips.top[panel$layout$ROW == 1] <-
    build_strip(panel, col_vars, facet$labeller, side = "top", ...)
  strips <- list(right = as.list(strips.right), top = as.list(strips.top))
  # the right/top element should exist if there are non-trivial labels
  # strips <- strips[sapply(strips, function(x) !identical(x, strips.empty))]
  strips$n <- list(top = 1, right = 1)
  strips
}

build_strip <- function(panel, label_df, labeller, side = "right", ...) {
  side <- match.arg(side, c("top", "left", "bottom", "right"))
  labeller <- match.fun(labeller)
  # No labelling data, so return empty string?
  if (plyr::empty(label_df)) {
    return("")
  }
  # Create matrix of labels
  labels <- matrix(list(), nrow = nrow(label_df), ncol = ncol(label_df))
  labels <- lapply(labeller(label_df), cbind)
  labels <- do.call("cbind", labels)
  # unlike ggplot2, we collapse "layers" of strips into 1 layer
  apply(labels, 1, paste, collapse = "; ")
}

#' @export
getStrips.wrap <- function(facet, panel, ...) {
  labels_df <- panel$layout[names(facet$facets)]
  labels_df[] <- plyr::llply(labels_df, format, justify = "none")
  # facet_wrap labels always go on top
  # we return a list so p_info.strips is always an object (on the JS side)
  strips <- list(top = apply(labels_df, 1, paste, collapse = ", "), right = list(""))
  # strips <- strips[!identical(strips$top, rep("", nrow(panel$layout)))]
  strips$n <- list(top = max(panel$layout$ROW), right = 0)
  strips
}

#' @export
getStrips.null <- function(facet, panel, ...) {
  list(top = list(""), right = list(""), n = list(top = 0, right = 0))
}

# Attach AXIS_X/AXIS_Y columns to the panel layout if
# facet_grids is used.
# Currently every axis is rendered,
# but this could be helpful if we decide not to that.
flag_axis <- function(facet, layout)
  UseMethod("flag_axis")

#' @export
flag_axis.grid <- function(facet, layout) {
  # 'grid rules' are to draw y-axis on panels with COL == 1
  # and ROW == max(ROW).
  layout$AXIS_Y <- layout$COL == 1
  layout$AXIS_X <- layout$ROW == max(layout$ROW)
  layout
}

#' @export
flag_axis.wrap <- function(facet, layout) {
  if (sum(grepl("^AXIS_[X-Y]$", names(layout))) != 2)
    stop("Expected 'AXIS_X' and 'AXIS_Y' to be in panel layout")
  layout
}

#' @export
flag_axis.null <- function(facet, layout) {
  cbind(layout, AXIS_X = TRUE, AXIS_Y = TRUE)
}

# TODO: how to 'train_layout' for non-cartesian coordinates?
# https://github.com/hadley/ggplot2/blob/dfcb56ec067910e1a3a04693d8f1e146cc7fb796/R/coord-.r
#' @keywords internal
train_layout <- function(facet, coord, layout, ranges) {
  npanels <- dim(layout)[1]
  nrows <- max(layout$ROW)
  ncols <- max(layout$COL)
  ydiffs <- sapply(ranges, function(z) diff(z$y.range))
  xdiffs <- sapply(ranges, function(z) diff(z$x.range))
  # if x or y scale is 'free', then ignore the ratio
  if (length(unique(xdiffs)) > 1 || length(unique(ydiffs)) > 1)
    coord$ratio <- NULL
  has.ratio <- !is.null(coord$ratio)
  layout$coord_fixed <- has.ratio
  if (has.ratio) {
    spaces <- fixed_spaces(ranges, coord$ratio)
    layout <- cbind(layout, width_proportion = spaces$x, height_proportion = spaces$y)
    layout$width_proportion <- layout$width_proportion/ncols
    layout$height_proportion <- layout$height_proportion/nrows
  } else {
    vars <- NULL
    if (isTRUE(facet$space_free$x)) vars <- c(vars, "x")
    if (isTRUE(facet$space_free$y)) vars <- c(vars, "y")
    if (is.null(vars)) { # fill the entire space and give panels equal area
      layout <- cbind(layout, width_proportion = rep(1/ncols, npanels),
                      height_proportion = rep(1/nrows, npanels))
    } else { #special handling for 'free' space
      for (xy in vars) {
        u.type <- toupper(xy)
        l.type <- tolower(xy)
        scale.type <- paste0("SCALE_", u.type)
        range.type <- paste0(l.type, ".range")
        space.type <- paste0("SPACE_", u.type)
        scale.vals <- layout[[scale.type]]
        uniq.scale.vals <- unique(scale.vals)
        diffs <- sapply(uniq.scale.vals, function(scale.value) {
          panel.value <- which(scale.vals==scale.value)[1]
          diff(ranges[[panel.value]][[range.type]])
        })
        # decide the proportion of the height/width each scale deserves based on the range
        props.list <- list()
        props.list[[scale.type]] <- uniq.scale.vals
        props.list[[space.type]] <- diffs / sum(diffs)
        props <- do.call(data.frame, props.list)
        layout <- plyr::join(layout, props, by = scale.type)
      }
      names(layout) <- gsub("SPACE_X", "width_proportion", names(layout), fixed = TRUE)
      names(layout) <- gsub("SPACE_Y", "height_proportion", names(layout), fixed = TRUE)
    }
  }
  layout
}

# fixed cartesian coordinates (on a 0-1 scale)
# inspired from https://github.com/hadley/ggplot2/blob/dfcb56ec067910e1a3a04693d8f1e146cc7fb796/R/coord-fixed.r#L34-36
fixed_spaces <- function(ranges, ratio = 1) {
  aspect <- sapply(ranges,
                   function(z) diff(z$y.range) / diff(z$x.range) * ratio)
  spaces <- list(y = aspect)
  spaces$x <- 1/spaces$y
  lapply(spaces, function(z) min(z, 1))
}
