#' Scales object encapsulates multiple scales.
#' All input and output done with data.frames to facilitate
#' multiple input and output variables
#' @export
a_scales_list <- function() {
  a_ggproto(NULL, a_ScalesList)
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_ScalesList <- a_ggproto("a_ScalesList", NULL,
  scales = NULL,

  find = function(self, a_aesthetic) {
    vapply(self$scales, function(x) any(a_aesthetic %in% x$a_aesthetics), logical(1))
  },

  has_scale = function(self, a_aesthetic) {
    any(self$find(a_aesthetic))
  },

  add = function(self, a_scale) {
    if (is.null(a_scale)) {
      return()
    }

    prev_aes <- self$find(a_scale$a_aesthetics)
    if (any(prev_aes)) {
      # Get only the first a_aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      scalename <- self$scales[prev_aes][[1]]$a_aesthetics[1]
      message_wrap("Scale for '", scalename,
        "' is already present. Adding another scale for '", scalename,
        "', which will replace the existing scale.")
    }

    # Remove old scale for this aesthetic (if it exists)
    self$scales <- c(self$scales[!prev_aes], list(a_scale))
  },

  n = function(self) {
    length(self$scales)
  },

  input = function(self) {
    unlist(lapply(self$scales, "[[", "a_aesthetics"))
  },

  # This actually makes a descendant of self, which is functionally the same
  # as a actually clone for most purposes.
  clone = function(self) {
    a_ggproto(NULL, self, scales = lapply(self$scales, function(s) s$clone()))
  },

  non_position_scales = function(self) {
    a_ggproto(NULL, self, scales = self$scales[!self$find("x") & !self$find("y")])
  },

  get_scales = function(self, output) {
    a_scale <- self$scales[self$find(output)]
    if (length(a_scale) == 0) return()
    a_scale[[1]]
  }
)

# Train scale from a data frame
scales_train_df <- function(scales, df, drop = FALSE) {
  if (empty(df) || length(scales$scales) == 0) return()

  lapply(scales$scales, function(a_scale) a_scale$train_df(df = df))
}

# Map values from a data.frame. Returns data.frame
scales_map_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  mapped <- unlist(lapply(scales$scales, function(a_scale) a_scale$map_df(df = df)), recursive = FALSE)

  plyr::quickdf(c(mapped, df[setdiff(names(df), names(mapped))]))
}

# Transform values to cardinal representation
scales_transform_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)),
    recursive = FALSE)
  plyr::quickdf(c(transformed, df[setdiff(names(df), names(transformed))]))
}

# @param aesthetics A list of aesthetic-variable mappings. The name of each
#   item is the aesthetic, and the value of each item is the variable in data.
scales_add_defaults <- function(scales, data, a_aesthetics, env) {
  if (is.null(a_aesthetics)) return()
  names(a_aesthetics) <- unlist(lapply(names(a_aesthetics), a_aes_to_scale))

  new_aesthetics <- setdiff(names(a_aesthetics), scales$input())
  # No new aesthetics, so no new scales to add
  if (is.null(new_aesthetics)) return()

  datacols <- plyr::tryapply(
    a_aesthetics[new_aesthetics], eval,
    envir = data, enclos = env
  )

  for (a_aes in names(datacols)) {
    scales$add(find_a_scale(a_aes, datacols[[a_aes]], env))
  }

}

# Add missing but required scales.
# @param aesthetics A character vector of aesthetics. Typically c("x", "y").
scales_add_missing <- function(plot, a_aesthetics, env) {

  # Keep only aesthetics that aren't already in plot$scales
  a_aesthetics <- setdiff(a_aesthetics, plot$scales$input())

  for (a_aes in a_aesthetics) {
    scale_name <- paste("a_scale", a_aes, "continuous", sep = "_")

    a_scale_f <- find_global(scale_name, env, mode = "function")
    plot$scales$add(a_scale_f())
  }
}


