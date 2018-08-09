# Panel object.
#
# A panel figures out how data is positioned within a panel of a plot,
# coordinates information from scales, facets and coords.  Eventually all
# state will move out of facets and coords, and live only in panels and
# stats, simplifying these data structures to become strategies.
#
# Information about a panel is built up progressively over time, which
# is why the initial object is empty to start with.
new_panel <- function() {
  structure(list(), class = "panel")
}

#' Learn the layout of panels within a plot.
#'
#' This is determined by the facet, which returns a data frame, than
#' when joined to the data to be plotted tells us which panel it should
#' appear in, where that panel appears in the grid, and what scales it
#' uses.
#'
#' As well as the layout info, this function also adds empty lists in which
#' to house the x and y scales.
#'
#' @param panel the panel object to train
#' @param a_facet the facetting specification
#' @param data a list of data frames (one for each layer), and one for the plot
#' @param plot_data the default data frame
#' @return an updated panel object
#' @keywords internal
a_train_layout <- function(panel, a_facet, data, plot_data) {
  layout <- a_facet_train_layout(a_facet, c(list(plot_data), data))
  panel$layout <- layout
  panel$shrink <- a_facet$shrink

  panel
}

#' Map data to find out where it belongs in the plot.
#'
#' Layout map ensures that all layer data has extra copies of data for margins
#' and missing facetting variables, and has a PANEL variable that tells that
#' so it know what panel it belongs to. This is a change from the previous
#' design which added facetting variables directly to the data frame and
#' caused problems when they had names of aesthetics (like colour or group).
#'
#' @param panel a trained panel object
#' @param the facetting specification
#' @param data list of data frames (one for each layer)
#' @keywords internal
map_layout <- function(panel, a_facet, data) {
  lapply(data, function(data) {
    a_facet_map_layout(a_facet, data, panel$layout)
  })
}

#' Train position scales with data
#'
#' If panel-specific scales are not already present, will clone from
#' the scales provided in the parameter
#'
#' @param panel the panel object to train
#' @param data a list of data frames (one for each layer)
#' @param x_scale x scale for the plot
#' @param y_scale y scale for the plot
#' @keywords internal
train_position <- function(panel, data, x_scale, y_scale) {
  # Initialise scales if needed, and possible.
  layout <- panel$layout
  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    panel$x_scales <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
  }
  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    panel$y_scales <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
  }

  # loop over each layer, training x and y scales in turn
  for (a_layer_data in data) {

    match_id <- match(a_layer_data$PANEL, layout$PANEL)

    if (!is.null(x_scale)) {
      x_vars <- intersect(x_scale$a_aesthetics, names(a_layer_data))
      SCALE_X <- layout$SCALE_X[match_id]

      scale_apply(a_layer_data, x_vars, "train", SCALE_X, panel$x_scales)
    }

    if (!is.null(y_scale)) {
      y_vars <- intersect(y_scale$a_aesthetics, names(a_layer_data))
      SCALE_Y <- layout$SCALE_Y[match_id]

      scale_apply(a_layer_data, y_vars, "train", SCALE_Y, panel$y_scales)
    }
  }

  panel
}


reset_scales <- function(panel) {
  if (!panel$shrink) return()
  lapply(panel$x_scales, function(s) s$reset())
  lapply(panel$y_scales, function(s) s$reset())
  invisible()
}

#' Map data with scales.
#'
#' This operation must be idempotent because it is applied twice: both before
#' and after statistical transformation.
#'
#' @param data a list of data frames (one for each layer)
#' @keywords internal
map_position <- function(panel, data, x_scale, y_scale) {
  layout <- panel$layout

  lapply(data, function(a_layer_data) {
    match_id <- match(a_layer_data$PANEL, layout$PANEL)

    # Loop through each variable, mapping across each scale, then joining
    # back together
    x_vars <- intersect(x_scale$a_aesthetics, names(a_layer_data))
    names(x_vars) <- x_vars
    SCALE_X <- layout$SCALE_X[match_id]
    new_x <- scale_apply(a_layer_data, x_vars, "map", SCALE_X, panel$x_scales)
    a_layer_data[, x_vars] <- new_x

    y_vars <- intersect(y_scale$a_aesthetics, names(a_layer_data))
    names(y_vars) <- y_vars
    SCALE_Y <- layout$SCALE_Y[match_id]
    new_y <- scale_apply(a_layer_data, y_vars, "map", SCALE_Y, panel$y_scales)

    a_layer_data[, y_vars] <- new_y
    a_layer_data
  })
}

# Function for applying scale method to multiple variables in a given
# data set.  Implement in such a way to minimize copying and hence maximise
# speed
scale_apply <- function(data, vars, method, scale_id, scales) {
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()

  n <- length(scales)
  if (any(is.na(scale_id))) stop()

  scale_index <- plyr::split_indices(scale_id, n)

  lapply(vars, function(var) {
    pieces <- lapply(seq_along(scales), function(i) {
      scales[[i]][[method]](data[[var]][scale_index[[i]]])
    })
    # Join pieces back together, if necessary
    if (!is.null(pieces)) {
      unlist(pieces)[order(unlist(scale_index))]
    }
  })
}


panel_scales <- function(panel, i) {
  this_panel <- panel$layout[panel$layout$PANEL == i, ]

  list(
    x = panel$x_scales[[this_panel$SCALE_X]],
    y = panel$y_scales[[this_panel$SCALE_Y]]
  )
}

# Compute ranges and dimensions of each panel, using the a_coord.
train_ranges <- function(panel, a_coord) {
  compute_range <- function(ix, iy) {
    # TODO: change a_coord_train method to take individual x and y scales
    a_coord$train(list(x = panel$x_scales[[ix]], y = panel$y_scales[[iy]]))
  }

  panel$ranges <- Map(compute_range,
    panel$layout$SCALE_X, panel$layout$SCALE_Y)
  panel
}

xlabel <- function(panel, a_labels) {
  panel$x_scales[[1]]$name %|W|% a_labels$x
}

ylabel <- function(panel, a_labels) {
  panel$y_scales[[1]]$name %|W|% a_labels$y
}
