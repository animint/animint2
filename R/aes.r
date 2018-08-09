#' @include utilities.r
NULL

.all_a_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "a_label", "linetype", "lower",
  "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
  "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
  "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")

.base_to_a_plot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)

#' Define aesthetic mappings.
#'
#' Generate aesthetic mappings that describe how variables in the data are
#' mapped to visual properties (aesthetics) of geoms. This function also
#' standardise aesthetic names by performs partial name matching, converting
#' color to colour, and old style R names to a_plot names (eg. pch to shape,
#' cex to size)
#'
#' @param x,y,... List of name value pairs giving aesthetics to map to
#'   variables. The names for x and y aesthetics can be omitted (because
#'   they are so common); all other aesthetics must be named.
#' @seealso See \code{\link{a_aes_q}}/\code{\link{a_aes_string}} for standard
#'   evaluation versions of \code{a_aes}.
#' @seealso See
#'    \code{\link{a_aes_colour_fill_alpha}}, \code{\link{a_aes_group_order}},
#'    \code{\link{a_aes_linetype_size_shape}} and \code{\link{a_aes_position}}
#'    for more specific examples with different aesthetics.
#' @export
#' @examples
#' a_aes(x = mpg, y = wt)
#' a_aes(mpg, wt)
#'
#' # You can also map aesthetics to functions of variables
#' a_aes(x = mpg ^ 2, y = wt / cyl)
#'
#' # Aesthetic names are automatically standardised
#' a_aes(col = x)
#' a_aes(fg = x)
#' a_aes(color = x)
#' a_aes(colour = x)
#'
#' # a_aes is almost always used with a_plot() or a layer
#' a_plot(mpg, a_aes(displ, hwy)) + a_geom_point()
#' a_plot(mpg) + a_geom_point(a_aes(displ, hwy))
#'
#' # a_aesthetics supplied to a_plot() are used as defaults for every layer
#' # you can override them, or supply different aesthetics for each layer
a_aes <- function(x, y, ...) {
  a_aes <- structure(as.list(match.call()[-1]), class = "uneval")
  rename_aes(a_aes)
}
#' @export
print.uneval <- function(x, ...) {
  values <- vapply(x, deparse2, character(1))
  bullets <- paste0("* ", format(names(x)), " -> ", values, "\n")

  cat(bullets, sep = "")
}

#' @export
str.uneval <- function(object, ...) utils::str(unclass(object), ...)
#' @export
"[.uneval" <- function(x, i, ...) structure(unclass(x)[i], class = "uneval")

#' @export
as.character.uneval <- function(x, ...) {
  char <- as.character(unclass(x))
  names(char) <- names(x)
  char
}

# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  # Convert prefixes to full names
  full <- match(names(x), .all_a_aesthetics)
  names(x)[!is.na(full)] <- .all_a_aesthetics[full[!is.na(full)]]

  plyr::rename(x, .base_to_a_plot, warn_missing = FALSE)
}

# Look up the scale that should be used for a given aesthetic
a_aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"

  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  a_aes_to_scale(vars) %in% c("x", "y")
}

#' Define aesthetic mappings from strings, or quoted calls and formulas.
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms. \code{\link{a_aes}} uses non-standard
#' evaluation to capture the variable names. \code{a_aes_} and \code{a_aes_string}
#' require you to explicitly quote the inputs either with \code{""} for
#' \code{a_aes_string()}, or with \code{quote} or \code{~} for \code{a_aes_()}.
#' (\code{a_aes_q} is an alias to \code{a_aes_})
#'
#' It's better to use \code{a_aes_q()}, because there's no easy way to create the
#' equivalent to \code{a_aes(colour = "my colour")} or \code{a_aes{x = `X$1`}}
#' with \code{a_aes_string()}.
#'
#' \code{a_aes_string} and \code{a_aes_} are particularly useful when writing
#' functions that create plots because you can use strings or quoted
#' names/calls to define the aesthetic mappings, rather than having to use
#' \code{\link{substitute}} to generate a call to \code{a_aes()}.
#'
#' @param x,y,... List of name value pairs. Elements must be either
#'   quoted calls, strings, one-sided formulas or constants.
#' @seealso \code{\link{a_aes}}
#' @export
#' @examples
#' # Three ways of generating the same aesthetics
#' a_aes(mpg, wt, col = cyl)
#' a_aes_(quote(mpg), quote(wt), col = quote(cyl))
#' a_aes_(~mpg, ~wt, col = ~cyl)
#' a_aes_string("mpg", "wt", col = "cyl")
#'
#' # You can't easily mimic these calls with a_aes_string
#' a_aes(`$100`, colour = "smooth")
#' a_aes_(~ `$100`, colour = "smooth")
#' # Ok, you can, but it requires a _lot_ of quotes
#' a_aes_string("`$100`", colour = '"smooth"')
#'
#' # Convert strings to names with as.name
#' var <- "cyl"
#' a_aes(col = x)
#' a_aes_(col = as.name(var))
a_aes_ <- function(x, y, ...) {
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  as_call <- function(x) {
    if (is.formula(x) && length(x) == 2) {
      x[[2]]
    } else if (is.call(x) || is.name(x) || is.atomic(x)) {
      x
    } else {
      stop("Aesthetic must be a one-sided formula, call, name, or constant.",
        call. = FALSE)
    }
  }
  mapping <- lapply(mapping, as_call)
  structure(rename_aes(mapping), class = "uneval")
}

#' @rdname a_aes_
#' @export
a_aes_string <- function(x, y, ...) {
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  mapping <- lapply(mapping, function(x) {
    if (is.character(x)) {
      parse(text = x)[[1]]
    } else {
      x
    }
  })
  structure(rename_aes(mapping), class = "uneval")
}

#' @export
#' @rdname a_aes_
a_aes_q <- a_aes_

#' Given a character vector, create a set of identity mappings
#'
#' @param vars vector of variable names
#' @keywords internal
#' @export
#' @examples
#' a_aes_all(names(mtcars))
#' a_aes_all(c("x", "y", "col", "pch"))
a_aes_all <- function(vars) {
  names(vars) <- vars
  vars <- rename_aes(vars)

  structure(
    lapply(vars, as.name),
    class = "uneval"
  )
}

#' Automatic aesthetic mapping
#'
#' @param data data.frame or names of variables
#' @param ... aesthetics that need to be explicitly mapped.
#' @keywords internal
#' @export
a_aes_auto <- function(data = NULL, ...) {
  warning("a_aes_auto() is deprecated", call. = FALSE)

  # detect names of data
  if (is.null(data)) {
    stop("a_aes_auto requires data.frame or names of data.frame.")
  } else if (is.data.frame(data)) {
    vars <- names(data)
  } else {
    vars <- data
  }

  # automatically detected aes
  vars <- intersect(.all_a_aesthetics, vars)
  names(vars) <- vars
  a_aes <- lapply(vars, function(x) parse(text = x)[[1]])

  # explicitly defined a_aes
  if (length(match.call()) > 2) {
    args <- as.list(match.call()[-1])
    a_aes <- c(a_aes, args[names(args) != "data"])
  }

  structure(rename_aes(a_aes), class = "uneval")
}
