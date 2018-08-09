#' Labeller functions
#'
#' Labeller functions are in charge of formatting the strip labels of
#' facet grids and wraps. Most of them accept a \code{multi_line}
#' argument to control whether multiple factors (defined in formulae
#' such as \code{~first + second}) should be displayed on a single
#' line separated with commas, or each on their own line.
#'
#' \code{a_label_value()} only displays the value of a factor while
#' \code{a_label_both()} displays both the variable name and the factor
#' value. \code{a_label_context()} is context-dependent and uses
#' \code{a_label_value()} for single factor facetting and
#' \code{a_label_both()} when multiple factors are
#' involved. \code{a_label_wrap_gen()} uses \code{\link[base]{strwrap}()}
#' for line wrapping.
#'
#' \code{a_label_parsed()} interprets the labels as plotmath
#' expressions. \code{\link{a_label_bquote}()} offers a more flexible
#' way of constructing plotmath expressions. See examples and
#' \code{\link{bquote}()} for details on the syntax of the
#' argument.
#'
#' @section Writing New Labeller Functions:
#'
#'   Note that an easy way to write a labeller function is to
#'   transform a function operating on character vectors with
#'   \code{\link{as_labeller}()}.
#'
#'   A labeller function accepts a data frame of labels (character
#'   vectors) containing one column for each factor. Multiple factors
#'   occur with formula of the type \code{~first + second}.
#'
#'   The return value must be a rectangular list where each 'row'
#'   characterises a single facet. The list elements can be either
#'   character vectors or lists of plotmath expressions. When multiple
#'   elements are returned, they get displayed on their own new lines
#'   (i.e., each facet gets a multi-line strip of labels).
#'
#'   To illustrate, let's say your labeller returns a list of two
#'   character vectors of length 3. This is a rectangular list because
#'   all elements have the same length. The first facet will get the
#'   first elements of each vector and display each of them on their
#'   own line. Then the second facet gets the second elements of each
#'   vector, and so on.
#'
#'   If it's useful to your labeller, you can retrieve the \code{type}
#'   attribute of the incoming data frame of labels. The value of this
#'   attribute reflects the kind of strips your labeller is dealing
#'   with: \code{"cols"} for columns and \code{"rows"} for rows. Note
#'   that \code{\link{a_facet_wrap}()} has columns by default and rows
#'   when the strips are switched with the \code{switch} option. The
#'   \code{a_facet} attribute also provides metadata on the labels. It
#'   takes the values \code{"grid"} or \code{"wrap"}.
#'
#'   For compatibility with \code{\link{labeller}()}, each labeller
#'   function must have the \code{labeller} S3 class.
#'
#' @param a_labels Data frame of labels. Usually contains only one
#'   element, but facetting over multiple factors entails multiple
#'   label variables.
#' @param multi_line Whether to display the labels of multiple factors
#'   on separate lines.
#' @param sep String separating variables and values.
#' @param width Maximum number of characters before wrapping the strip.
#' @family a_facet
#' @seealso \code{\link{labeller}()}, \code{\link{as_labeller}()},
#'   \code{\link{a_label_bquote}()}
#' @name labellers
#' @examples
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#' p <- a_plot(mtcars, a_aes(wt, mpg)) + a_geom_point()
#'
#' # Displaying only the values
#' p + a_facet_grid(. ~ cyl)
#' p + a_facet_grid(. ~ cyl, labeller = a_label_value)
#'
#' \donttest{
#' # Displaying both the values and the variables
#' p + facet_grid(. ~ cyl, labeller = a_label_both)
#'
#' # Displaying only the values or both the values and variables
#' # depending on whether multiple factors are facetted over
#' p + facet_grid(am ~ vs+cyl, labeller = a_label_context)
#'
#' # Interpreting the labels as plotmath expressions
#' p + a_facet_grid(. ~ cyl2)
#' p + a_facet_grid(. ~ cyl2, labeller = a_label_parsed)
#' p + a_facet_wrap(~vs + cyl2, labeller = a_label_parsed)
#' }
NULL

collapse_labels_lines <- function(a_labels) {
  out <- do.call("Map", c(list(paste, sep = ", "), a_labels))
  list(unname(unlist(out)))
}

#' @rdname labellers
#' @export
a_label_value <- function(a_labels, multi_line = TRUE) {
  a_labels <- lapply(a_labels, as.character)
  if (multi_line) {
    a_labels
  } else {
    collapse_labels_lines(a_labels)
  }
}
# Should ideally not have the 'function' class here, but this is
# currently needed for Roxygen
class(a_label_value) <- c("function", "labeller")

# Helper for a_label_both
a_label_variable <- function(a_labels, multi_line = TRUE) {
  if (multi_line) {
    row <- as.list(names(a_labels))
  } else {
    row <- list(paste(names(a_labels), collapse = ", "))
  }
  lapply(row, rep, nrow(a_labels) %||% length(a_labels[[1]]))
}

#' @rdname labellers
#' @export
a_label_both <- function(a_labels, multi_line = TRUE, sep = ": ") {
  value <- a_label_value(a_labels, multi_line = multi_line)
  variable <- a_label_variable(a_labels, multi_line = multi_line)

  if (multi_line) {
    out <- vector("list", length(value))
    for (i in seq_along(out)) {
      out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
    }
  } else {
    value <- do.call("paste", c(value, sep = ", "))
    variable <- do.call("paste", c(variable, sep = ", "))
    out <- Map(paste, variable, value, sep = sep)
    out <- list(unname(unlist(out)))
  }

  out
}
class(a_label_both) <- c("function", "labeller")

#' @rdname labellers
#' @export
a_label_context <- function(a_labels, multi_line = TRUE, sep = ": ") {
  if (length(a_labels) == 1) {
    a_label_value(a_labels, multi_line)
  } else {
    a_label_both(a_labels, multi_line)
  }
}
class(a_label_context) <- c("function", "labeller")

#' @rdname labellers
#' @export
a_label_parsed <- function(a_labels, multi_line = TRUE) {
  a_labels <- a_label_value(a_labels, multi_line = multi_line)
  if (multi_line) {
    # Using unname() and c() to return a cleaner and easily testable
    # object structure
    lapply(unname(a_labels), lapply, function(values) {
      c(parse(text = as.character(values)))
    })
  } else {
    lapply(a_labels, function(values) {
      values <- paste0("list(", values, ")")
      lapply(values, function(expr) c(parse(text = expr)))
    })
  }
}
class(a_label_parsed) <- c("function", "labeller")

find_names <- function(expr) {
  if (is.call(expr)) {
    unlist(lapply(expr[-1], find_names))
  } else if (is.name(expr)) {
    as.character(expr)
  }
}

#' Backquoted labeller
#'
#' \code{\link{a_label_bquote}()} offers a flexible way of labelling
#' facet rows or columns with plotmath expressions. Backquoted
#' variables will be replaced with their value in the facet.
#' @param rows Backquoted labelling expression for rows.
#' @param cols Backquoted labelling expression for columns.
#' @param default Default labeller function for the rows or the
#'   columns when no plotmath expression is provided.
#' @seealso \link{labellers}, \code{\link{labeller}()},
#' @export
#' @examples
#' # The variables mentioned in the plotmath expression must be
#' # backquoted and referred to by their names.
#' p <- a_plot(mtcars, a_aes(wt, mpg)) + a_geom_point()
#' p + a_facet_grid(vs ~ ., labeller = a_label_bquote(alpha ^ .(vs)))
#' p + a_facet_grid(. ~ vs, labeller = a_label_bquote(cols = .(vs) ^ .(vs)))
#' p + a_facet_grid(. ~ vs + am, labeller = a_label_bquote(cols = .(am) ^ .(vs)))
a_label_bquote <- function(rows = NULL, cols = NULL,
                         default = a_label_value) {
  cols_quoted <- substitute(cols)
  rows_quoted <- substitute(rows)
  has_warned <- FALSE

  fun <- function(a_labels) {
    quoted <- resolve_labeller(rows_quoted, cols_quoted, a_labels)
    if (is.null(quoted)) {
      return(a_label_value(a_labels))
    }

    evaluate <- function(...) {
      params <- list(...)

      # Mapping `x` to the first variable for backward-compatibility,
      # but only if there is no facetted variable also named `x`
      if ("x" %in% find_names(quoted) && !"x" %in% names(params)) {
        if (!has_warned) {
          warning("Referring to `x` is deprecated, use variable name instead",
            call. = FALSE)
          # The function is called for each facet so this avoids
          # multiple warnings
          has_warned <<- TRUE
        }
        params$x <- params[[1]]
      }

      eval(substitute(bquote(expr, params), list(expr = quoted)))
    }
    list(do.call("Map", c(list(f = evaluate), a_labels)))
  }

  structure(fun, class = "labeller")
}
globalVariables(c("x", "."))

#' @rdname labellers
#' @export
a_label_wrap_gen <- function(width = 25, multi_line = TRUE) {
  fun <- function(a_labels) {
    a_labels <- a_label_value(a_labels, multi_line = multi_line)
    lapply(a_labels, function(x) {
      x <- strwrap(x, width = width, simplify = FALSE)
      vapply(x, paste, character(1), collapse = "\n")
    })
  }
  structure(fun, class = "labeller")
}

is_labeller <- function(x) inherits(x, "labeller")

resolve_labeller <- function(rows, cols, a_labels) {
  if (is.null(cols) && is.null(rows)) {
    stop("Supply one of rows or cols", call. = FALSE)
  }
  if (attr(a_labels, "a_facet") == "wrap") {
    # Return either rows or cols for facet_wrap()
    if (!is.null(cols) && !is.null(rows)) {
      stop("Cannot supply both rows and cols to a_facet_wrap()", call. = FALSE)
    }
    cols %||% rows
  } else {
    if (attr(a_labels, "type") == "rows") {
      rows
    } else {
      cols
    }
  }
}

#' Coerce to labeller function
#'
#' This transforms objects to labeller functions. Used internally by
#' \code{\link{labeller}()}.
#' @param x Object to coerce to a labeller function. If a named
#'   character vector, it is used as a lookup table before being
#'   passed on to \code{default}. If a non-labeller function, it is
#'   assumed it takes and returns character vectors and is applied to
#'   the labels. If a labeller, it is simply applied to the labels.
#' @param multi_line Whether to display the labels of multiple factors
#'   on separate lines. This is passed to the labeller function.
#' @param default Default labeller to process the labels produced by
#'   lookup tables or modified by non-labeller functions.
#' @seealso \code{\link{labeller}()}, \link{labellers}
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(disp, drat)) + a_geom_point()
#' p + a_facet_wrap(~am)
#'
#' # Rename labels on the fly with a lookup character vector
#' to_string <- as_labeller(c(`0` = "Zero", `1` = "One"))
#' p + a_facet_wrap(~am, labeller = to_string)
#'
#' # Quickly transform a function operating on character vectors to a
#' # labeller function:
#' appender <- function(string, suffix = "-foo") paste0(string, suffix)
#' p + a_facet_wrap(~am, labeller = as_labeller(appender))
#'
#' # If you have more than one facetting variable, be sure to dispatch
#' # your labeller to the right variable with labeller()
#' p + a_facet_grid(cyl ~ am, labeller = labeller(am = to_string))
as_labeller <- function(x, default = a_label_value, multi_line = TRUE) {
  force(x)
  fun <- function(a_labels) {
    a_labels <- lapply(a_labels, as.character)

    # Dispatch multi_line argument to the labeller function instead of
    # supplying it to the labeller call because some labellers do not
    # support it.
    default <- dispatch_args(default, multi_line = multi_line)

    if (is_labeller(x)) {
      x <- dispatch_args(x, multi_line = multi_line)
      x(a_labels)
    } else if (is.function(x)) {
      default(lapply(a_labels, x))
    } else if (is.character(x)) {
      default(lapply(a_labels, function(a_label) x[a_label]))
    } else {
      default(a_labels)
    }
  }
  structure(fun, class = "labeller")
}

#' Generic labeller function for facets
#'
#' This function makes it easy to assign different labellers to
#' different factors. The labeller can be a function or it can be a
#' named character vectors that will serve as a lookup table.
#'
#' In case of functions, if the labeller has class \code{labeller}, it
#' is directly applied on the data frame of labels. Otherwise, it is
#' applied to the columns of the data frame of labels. The data frame
#' is then processed with the function specified in the
#' \code{.default} argument. This is intended to be used with
#' functions taking a character vector such as
#' \code{\link[Hmisc]{capitalize}}.
#'
#' @param ... Named arguments of the form \code{variable =
#'   labeller}. Each labeller is passed to \code{\link{as_labeller}()}
#'   and can be a lookup table, a function taking and returning
#'   character vectors, or simply a labeller function.
#' @param .rows,.cols Labeller for a whole margin (either the rows or
#'   the columns). It is passed to \code{\link{as_labeller}()}. When a
#'   margin-wide labeller is set, make sure you don't mention in
#'   \code{...} any variable belonging to the margin.
#' @param keep.as.numeric Deprecated. All supplied labellers and
#'   on-labeller functions should be able to work with character
#'   labels.
#' @param .multi_line Whether to display the labels of multiple
#'   factors on separate lines. This is passed to the labeller
#'   function.
#' @param .default Default labeller for variables not specified. Also
#'   used with lookup tables or non-labeller functions.
#' @family a_facet labeller
#' @seealso \code{\link{as_labeller}()}, \link{labellers}
#' @return A labeller function to supply to \code{\link{a_facet_grid}}
#'   for the argument \code{labeller}.
#' @export
#' @examples
#' \donttest{
#' p1 <- a_plot(mtcars, a_aes(x = mpg, y = wt)) + a_geom_point()
#'
#' # You can assign different labellers to variables:
#' p1 + facet_grid(vs + am ~ gear,
#'   labeller = labeller(vs = a_label_both, am = a_label_value))
#'
#' # Or whole margins:
#' p1 + facet_grid(vs + am ~ gear,
#'   labeller = labeller(.rows = a_label_both, .cols = a_label_value))
#'
#' # You can supply functions operating on strings:
#' capitalize <- function(string) {
#'   substr(string, 1, 1) <- toupper(substr(string, 1, 1))
#'   string
#' }
#' p2 <- a_plot(msleep, a_aes(x = sleep_total, y = awake)) + a_geom_point()
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(vore = capitalize))
#'
#' # Or use character vectors as lookup tables:
#' conservation_status <- c(
#'   cd = "Conservation Dependent",
#'   en = "Endangered",
#'   lc = "Least concern",
#'   nt = "Near Threatened",
#'   vu = "Vulnerable",
#'   domesticated = "Domesticated"
#' )
#' ## Source: http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#'
#' p2 + facet_grid(vore ~ conservation, labeller = labeller(
#'   .default = capitalize,
#'   conservation = conservation_status
#' ))
#'
#' # In the following example, we rename the levels to the long form,
#' # then apply a wrap labeller to the columns to prevent cropped text
#' msleep$conservation2 <- plyr::revalue(msleep$conservation,
#'   conservation_status)
#'
#' p2 %+% msleep + facet_grid(vore ~ conservation2)
#' p2 %+% msleep +
#'   facet_grid(vore ~ conservation2,
#'     labeller = labeller(conservation2 = a_label_wrap_gen(10))
#'   )
#'
#' # labeller() is especially useful to act as a global labeller. You
#' # can set it up once and use it on a range of different plots with
#' # different facet specifications.
#'
#' global_labeller <- labeller(
#'   vore = capitalize,
#'   conservation = conservation_status,
#'   conservation2 = a_label_wrap_gen(10),
#'   .default = a_label_both
#' )
#'
#' p2 + facet_grid(vore ~ conservation, labeller = global_labeller)
#' p2 + facet_wrap(~vore, labeller = global_labeller)
#' p2 %+% msleep + facet_wrap(~conservation2, labeller = global_labeller)
#' }
labeller <- function(..., .rows = NULL, .cols = NULL,
                     keep.as.numeric = NULL, .multi_line = TRUE,
                     .default = a_label_value) {
  if (!is.null(keep.as.numeric)) {
    .Deprecated(old = "keep.as.numeric")
  }
  dots <- list(...)
  .default <- as_labeller(.default)

  function(a_labels) {
    if (!is.null(.rows) || !is.null(.cols)) {
      margin_labeller <- resolve_labeller(.rows, .cols, a_labels)
    } else {
      margin_labeller <- NULL
    }

    if (is.null(margin_labeller)) {
      labellers <- lapply(dots, as_labeller)
    } else {
      margin_labeller <- as_labeller(margin_labeller, default = .default,
        multi_line = .multi_line)

      # Check that variable-specific labellers do not overlap with
      # margin-wide labeller
      if (any(names(dots) %in% names(a_labels))) {
        stop("Conflict between .", attr(a_labels, "type"), " and ",
          paste(names(dots), collapse = ", "), call. = FALSE)
      }
    }

    # Apply relevant labeller
    if (is.null(margin_labeller)) {
      # Apply named labeller one by one
      out <- lapply(names(a_labels), function(a_label) {
        if (a_label %in% names(labellers)) {
          labellers[[a_label]](a_labels[a_label])[[1]]
        } else {
          .default(a_labels[a_label])[[1]]
        }
      })
      names(out) <- names(a_labels)
      if (.multi_line) {
        out
      } else {
        collapse_labels_lines(out)
      }
    } else {
      margin_labeller(a_labels)
    }
  }
}

#' a_build strip function
#' @param panel ....
#' @param a_label_df ....
#' @param labeller ....
#' @param a_theme .....
#' @param side .....
#' @param switch ....
#' @export
## TODO: define params in detail
a_build_strip <- function(panel, a_label_df, labeller, a_theme, side = "right", switch = NULL) {
  side <- match.arg(side, c("top", "left", "bottom", "right"))
  horizontal <- side %in% c("top", "bottom")
  labeller <- match.fun(labeller)

  # No labelling data, so return empty row/col
  if (empty(a_label_df)) {
    if (horizontal) {
      widths <- unit(rep(0, max(panel$layout$COL)), "null")
      return(gtable_row_spacer(widths))
    } else {
      heights <- unit(rep(0, max(panel$layout$ROW)), "null")
      return(gtable_col_spacer(heights))
    }
  }

  # Create matrix of a_labels
  a_labels <- lapply(labeller(a_label_df), cbind)
  a_labels <- do.call("cbind", a_labels)

  # Display the mirror of the y strip a_labels if switched
  if (!is.null(switch) && switch %in% c("both", "y")) {
    a_theme$strip.text.y$angle <- adjust_angle(a_theme$strip.text.y$angle)
  }

  # Render as grobs
  grobs <- apply(a_labels, c(1, 2), ggstrip, a_theme = a_theme,
    horizontal = horizontal)

  # Create layout
  name <- paste("strip", side, sep = "-")
  if (horizontal) {
    # Each row is as high as the highest and as a wide as the panel
    row_height <- function(row) max(plyr::laply(row, height_cm))
    grobs <- t(grobs)
    heights <- unit(apply(grobs, 1, row_height), "cm")
    widths <- unit(rep(1, ncol(grobs)), "null")
  } else {
    # Each row is wide as the widest and as high as the panel
    col_width <- function(col) max(plyr::laply(col, width_cm))
    widths <- unit(apply(grobs, 2, col_width), "cm")
    heights <- unit(rep(1, nrow(grobs)), "null")
  }
  gtable_matrix(name, grobs, heights = heights, widths = widths)
}

# Grob for strip a_labels
ggstrip <- function(text, horizontal = TRUE, a_theme) {
  text_a_theme <- if (horizontal) "strip.text.x" else "strip.text.y"
  if (is.list(text)) text <- text[[1]]

  a_element <- calc_element(text_a_theme, a_theme)
  if (inherits(a_element, "a_element_blank"))
    return(a_zeroGrob())

  gp <- gpar(fontsize = a_element$size, col = a_element$colour,
    fontfamily = a_element$family, fontface = a_element$face,
    lineheight = a_element$lineheight)

  a_label <- stripGrob(text, a_element$hjust, a_element$vjust, a_element$angle,
    margin = a_element$margin, gp = gp, debug = a_element$debug)

  ggname("strip", absoluteGrob(
    gList(
      a_element_render(a_theme, "strip.background"),
      a_label
    ),
    width = grobWidth(a_label),
    height = grobHeight(a_label)
  ))

}

# Helper to adjust angle of switched strips
adjust_angle <- function(angle) {
  if (is.null(angle)) {
    -90
  } else if ((angle + 180) > 360) {
    angle - 180
  } else {
    angle + 180
  }
}

# Check for old school labeller
check_labeller <- function(labeller) {
  labeller <- match.fun(labeller)
  is_deprecated <- all(c("variable", "value") %in% names(formals(labeller)))

  if (is_deprecated) {
    old_labeller <- labeller
    labeller <- function(a_labels) {
      Map(old_labeller, names(a_labels), a_labels)
    }
    warning("The labeller API has been updated. Labellers taking `variable`",
      "and `value` arguments are now deprecated. See labellers documentation.",
      call. = FALSE)
  }

  labeller
}
