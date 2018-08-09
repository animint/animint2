#' Create a new layer
#'
#' A layer is a combination of data, stat and a_geom with a potential position
#' adjustment. Usually layers are created using \code{a_geom_*} or \code{a_stat_*}
#' calls but it can also be created directly using this function.
#'
#' @export
#' @inheritParams a_geom_point
#' @param mapping Set of aesthetic mappings created by \code{\link{a_aes}} or
#'   \code{\link{a_aes_}}. If specified and \code{inherit.a_aes = TRUE} (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If \code{NULL}, the default, the data is inherited from the plot
#'    data as specified in the call to \code{\link{a_plot}}.
#'
#'    A \code{data.frame}, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    \code{\link{a_fortify}} for which variables will be created.
#'
#'    A \code{function} will be called with a single argument,
#'    the plot data. The return value must be a \code{data.frame.}, and
#'    will be used as the layer data.
#' @param a_geom The geometric object to use display the data
#' @param a_stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param a_position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.a_aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param params Additional parameters to the \code{a_geom} and \code{a_stat}.
#' @param subset DEPRECATED. An older way of subsetting the dataset used in a
#'   layer.
#' @examples
#' # geom calls are just a short cut for layer
#' a_plot(mpg, a_aes(displ, hwy)) + a_geom_point()
#' # shortcut for
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_layer(a_geom = "point", a_stat = "identity", a_position = "identity",
#'     params = list(na.rm = FALSE)
#'   )
#'
#' # use a function as data to plot a subset of global data
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_layer(a_geom = "point", a_stat = "identity", a_position = "identity",
#'     data = head, params = list(na.rm = FALSE)
#'   )
#'
a_layer <- function(a_geom = NULL, a_stat = NULL,
                  data = NULL, mapping = NULL,
                  a_position = NULL, params = list(),
                  inherit.a_aes = TRUE, subset = NULL, show.legend = NA) {
  if (is.null(a_geom))
    stop("Attempted to create layer with no geom.", call. = FALSE)
  if (is.null(a_stat))
    stop("Attempted to create layer with no stat.", call. = FALSE)
  if (is.null(a_position))
    stop("Attempted to create layer with no position.", call. = FALSE)

  # Handle show_guide/show.legend
  if (!is.null(params$show_guide)) {
    warning("`show_guide` has been deprecated. Please use `show.legend` instead.",
      call. = FALSE)
    show.legend <- params$show_guide
    params$show_guide <- NULL
  }
  if (!is.logical(show.legend) || length(show.legend) != 1) {
    warning("`show.legend` must be a logical vector of length 1.", call. = FALSE)
    show.legend <- FALSE
  }

  data <- a_fortify(data)
  if (!is.null(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping must be created by `a_aes()` or `a_aes_()`", call. = FALSE)
  }

  if (is.character(a_geom))
    a_geom <- find_subclass("a_Geom", a_geom)
  if (is.character(a_stat))
    a_stat <- find_subclass("a_Stat", a_stat)
  if (is.character(a_position))
    a_position <- find_subclass("a_Position", a_position)

  # Special case for na.rm parameter needed by all layers
  if (is.null(params$na.rm)) {
    params$na.rm <- FALSE
  }

  # Split up params between aesthetics, geom, and stat
  params <- rename_aes(params)
  a_aes_params  <- params[intersect(names(params), a_geom$a_aesthetics())]
  a_geom_params <- params[intersect(names(params), a_geom$parameters(TRUE))]
  a_stat_params <- params[intersect(names(params), a_stat$parameters(TRUE))]

  all <- c(a_geom$parameters(TRUE), a_stat$parameters(TRUE), a_geom$a_aesthetics())
  extra <- setdiff(names(params), all)

  # Handle extra params
  if (is.null(params$validate_params)) {
    ## If validate_params has not been defined, default is set to TRUE
    ## TODO: Since we don't have to worry about ggplot2 compatability now,
    ## we could get rid of this altogether for a better implementation??
    params$validate_params <- FALSE
    extra_params <- NULL
  }

  if (length(extra) > 0 && params$validate_params) {
    stop("Unknown parameters: ", paste(extra, collapse = ", "), call. = FALSE)
  }else if (length(extra) > 0) {
    extra <- extra[!extra == "validate_params"]
    extra_params <- params[extra]
  }

  a_ggproto("a_LayerInstance", a_Layer,
    a_geom = a_geom,
    a_geom_params = a_geom_params,
    a_stat = a_stat,
    a_stat_params = a_stat_params,
    data = data,
    mapping = mapping,
    a_aes_params = a_aes_params,
    subset = subset,
    a_position = a_position,
    inherit.a_aes = inherit.a_aes,
    show.legend = show.legend,
    extra_params = extra_params
  )
}

a_Layer <- a_ggproto("a_Layer", NULL,
  a_geom = NULL,
  a_geom_params = NULL,
  a_stat = NULL,
  a_stat_params = NULL,
  data = NULL,
  a_aes_params = NULL,
  mapping = NULL,
  a_position = NULL,
  inherit.a_aes = FALSE,
  extra_params = NULL,
  print = function(self) {
    if (!is.null(self$mapping)) {
      cat("mapping:", clist(self$mapping), "\n")
    }
    cat(snakeize(class(self$a_geom)[[1]]), ": ", clist(self$a_geom_params), "\n",
      sep = "")
    cat(snakeize(class(self$a_stat)[[1]]), ": ", clist(self$a_stat_params), "\n",
      sep = "")
    cat(snakeize(class(self$a_position)[[1]]), "\n")
  },

  a_layer_data = function(self, plot_data) {
    if (is.waive(self$data)) {
      plot_data
    } else if (is.function(self$data)) {
      data <- self$data(plot_data)
      if (!is.data.frame(data)) {
        stop("Data function must return a data.frame", call. = FALSE)
      }
      data
    } else {
      self$data
    }
  },

  compute_aesthetics = function(self, data, plot) {
    # For annotation geoms, it is useful to be able to ignore the default aes
    if (self$inherit.a_aes) {
      a_aesthetics <- defaults(self$mapping, plot$mapping)
    } else {
      a_aesthetics <- self$mapping
    }

    # Drop aesthetics that are set or calculated
    set <- names(a_aesthetics) %in% names(self$a_aes_params)
    calculated <- is_calculated_aes(a_aesthetics)
    a_aesthetics <- a_aesthetics[!set & !calculated]

    # Override grouping if set in layer
    if (!is.null(self$a_geom_params$group)) {
      a_aesthetics[["group"]] <- self$a_aes_params$group
    }

    # Old subsetting method
    if (!is.null(self$subset)) {
      include <- data.frame(plyr::eval.quoted(self$subset, data, plot$env))
      data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
    }

    scales_add_defaults(plot$scales, data, a_aesthetics, plot$plot_env)

    # Evaluate and check a_aesthetics
    a_aesthetics <- compact(a_aesthetics)
    evaled <- lapply(a_aesthetics, eval, envir = data, enclos = plot$plot_env)

    n <- nrow(data)
    if (n == 0) {
      # No data, so look at longest evaluated aesthetic
      if (length(evaled) == 0) {
        n <- 0
      } else {
        n <- max(vapply(evaled, length, integer(1)))
      }
    }
    check_aesthetics(evaled, n)

    # Set special group and panel vars
    if (empty(data) && n > 0) {
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    evaled <- lapply(evaled, unname)
    evaled <- data.frame(evaled, stringsAsFactors = FALSE)
    evaled <- add_group(evaled)
    evaled
  },

  compute_statistic = function(self, data, panel) {
    if (empty(data))
      return(data.frame())

    params <- self$a_stat$setup_params(data, self$a_stat_params)
    data <- self$a_stat$setup_data(data, params)
    self$a_stat$compute_layer(data, params, panel)
  },

  map_statistic = function(self, data, plot) {
    if (empty(data)) return(data.frame())

    # Assemble aesthetics from layer, plot and stat mappings
    a_aesthetics <- self$mapping
    if (self$inherit.a_aes) {
      a_aesthetics <- defaults(a_aesthetics, plot$mapping)
    }
    a_aesthetics <- defaults(a_aesthetics, self$a_stat$default_aes)
    a_aesthetics <- compact(a_aesthetics)

    new <- strip_dots(a_aesthetics[is_calculated_aes(a_aesthetics)])
    if (length(new) == 0) return(data)

    # Add map stat output to a_aesthetics
    a_stat_data <- plyr::quickdf(lapply(new, eval, data, baseenv()))
    names(a_stat_data) <- names(new)

    # Add any new scales, if needed
    scales_add_defaults(plot$scales, data, new, plot$plot_env)
    # Transform the values, if the scale say it's ok
    # (see a_stat_spoke for one exception)
    if (self$a_stat$retransform) {
      a_stat_data <- scales_transform_df(plot$scales, a_stat_data)
    }

    cunion(a_stat_data, data)
  },

  compute_geom_1 = function(self, data) {
    if (empty(data)) return(data.frame())
    data <- self$a_geom$setup_data(data, c(self$a_geom_params, self$a_aes_params))

    check_required_aesthetics(
      self$a_geom$required_aes,
      c(names(data), names(self$a_aes_params)),
      snake_class(self$a_geom)
    )

    data
  },

  compute_position = function(self, data, panel) {
    if (empty(data)) return(data.frame())

    params <- self$a_position$setup_params(data)
    data <- self$a_position$setup_data(data, params)

    self$a_position$compute_layer(data, params, panel)
  },

  compute_geom_2 = function(self, data) {
    # Combine aesthetics, defaults, & params
    if (empty(data)) return(data)

    self$a_geom$use_defaults(data, self$a_aes_params)
  },

  draw_geom = function(self, data, panel, a_coord) {
    if (empty(data)) {
      n <- nrow(panel$layout)
      return(rep(list(a_zeroGrob()), n))
    }

    data <- self$a_geom$handle_na(data, self$a_geom_params)
    self$a_geom$draw_layer(data, self$a_geom_params, panel, a_coord)
  }
)

is.a_layer <- function(x) inherits(x, "a_Layer")


find_subclass <- function(super, class) {
  name <- paste0(super, camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }

  obj
}
