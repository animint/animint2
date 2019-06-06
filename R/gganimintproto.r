#' Create a new gganimintproto object
#'
#' gganimintproto is inspired by the proto package, but it has some important
#' differences. Notably, it cleanly supports cross-package inheritance, and has
#' faster performance.
#'
#' @section Calling gganimintproto methods:
#'
#' gganimintproto methods can take an optional \code{self} argument: if it is present,
#' it is a regular method; if it's absent, it's a "static" method (i.e. it
#' doesn't use any fields).
#'
#' Imagine you have a gganimintproto object \code{Adder}, which has a
#' method \code{addx = function(self, n) n + self$x}. Then, to call this
#' function, you would use \code{Adder$addx(10)} -- the \code{self} is passed
#' in automatically by the wrapper function. \code{self} be located anywhere
#' in the function signature, although customarily it comes first.
#'
#' @section Calling methods in a parent:
#'
#' To explicitly call a methods in a parent, use
#' \code{gganimintproto_parent(Parent, self)}.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. If \code{NULL} (the default), no class name will
#'   be added to the object.
#' @param _inherit gganimintproto object to inherit from. If \code{NULL}, don't inherit
#'   from any object.
#' @param parent,self Access parent class \code{parent} of object \code{self}.
#' @param ... A list of members in the gganimintproto object.
#' @export
gganimintproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  e <- new.env(parent = emptyenv())

  members <- list(...)
  if (length(members) != sum(nzchar(names(members)))) {
    stop("All members of a gganimintproto object must be named.")
  }

  # R <3.1.2 will error when list2env() is given an empty list, so we need to
  # check length. https://github.com/hadley/ggplot2/issues/1444
  if (length(members) > 0) {
    list2env(members, envir = e)
  }

  if (!is.null(`_inherit`)) {
    if (!is.gganimintproto(`_inherit`)) {
      stop("`_inherit` must be a gganimintproto object.")
    }
    e$super <- `_inherit`
    class(e) <- c(`_class`, class(`_inherit`))

  } else {
    class(e) <- c(`_class`, "gganimintproto")
  }

  e
}

#' Is an object a gganimintproto object?
#'
#' @param x An object to test.
#' @export
is.gganimintproto <- function(x) inherits(x, "gganimintproto")

fetch_gganimintproto <- function(x, name) {
  res <- NULL

  val <- .subset2(x, name)
  # The is.null check is an optimization for a common case; exists() also
  # catches the case where the value exists but has a NULL value.
  if (!is.null(val) || exists(name, envir = x, inherits = FALSE)) {
    res <- val
  } else {
    # If not found here, recurse into super environments
    super <- .subset2(x, "super")
    if (is.gganimintproto(super))
      res <- fetch_gganimintproto(super, name)
  }

  res
}

#' @export
#' @rdname gganimintproto
gganimintproto_parent <- function(parent, self) {
  structure(list(parent = parent, self = self), class = "gganimintproto_parent")
}

#' @export
`$.gganimintproto` <- function(x, name) {
  res <- fetch_gganimintproto(x, name)
  if (!is.function(res)) {
    return(res)
  }

  make_proto_method(x, res)
}

#' @export
`$.gganimintproto_parent` <- function(x, name) {
  res <- fetch_gganimintproto(.subset2(x, "parent"), name)
  if (!is.function(res)) {
    return(res)
  }

  make_proto_method(.subset2(x, "self"), res)
}

make_proto_method <- function(self, f) {
  args <- formals(f)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where there's a `self = NULL` argument.
  has_self  <- !is.null(args[["self"]]) || "self"  %in% names(args)

  if (has_self) {
    fun <- function(...) f(..., self = self)
  } else {
    fun <- function(...) f(...)
  }

  class(fun) <- "gganimintproto_method"
  fun
}


#' @export
`[[.gganimintproto` <- `$.gganimintproto`

#' Convert a gganimintproto object to a list
#'
#' This will not include the object's \code{super} member.
#'
#' @param x A gganimintproto object to convert to a list.
#' @param inherit If \code{TRUE} (the default), flatten all inherited items into
#'   the returned list. If \code{FALSE}, do not include any inherited items.
#' @param ... Further arguments to pass to \code{as.list.environment}.
#' @export
as.list.gganimintproto <- function(x, inherit = TRUE, ...) {
  res <- list()

  if (inherit) {
    if (!is.null(x$super)) {
      res <- as.list(x$super)
    }
  }

  current <- as.list.environment(x, ...)
  res[names(current)] <- current
  res$super <- NULL
  res
}


#' Print a gganimintproto object
#'
#' If a gganimintproto object has a \code{$print} method, this will call that method.
#' Otherwise, it will print out the members of the object, and optionally, the
#' members of the inherited objects.
#'
#' @param x A gganimintproto object to print.
#' @param flat If \code{TRUE} (the default), show a flattened list of all local
#'   and inherited members. If \code{FALSE}, show the inheritance hierarchy.
#' @param ... If the gganimintproto object has a \code{print} method, further arguments
#'   will be passed to it. Otherwise, these arguments are unused.
#'
#' @export
print.gganimintproto <- function(x, ..., flat = TRUE) {
  if (is.function(x$print)) {
    x$print(...)

  } else {
    cat(format(x, flat = flat), "\n", sep = "")
    invisible(x)
  }
}


#' Format a gganimintproto object
#'
#' @inheritParams print.gganimintproto
#' @export
format.gganimintproto <-  function(x, ..., flat = TRUE) {
  classes_str <- function(obj) {
    classes <- setdiff(class(obj), "gganimintproto")
    if (length(classes) == 0)
      return("")
    paste0(": Class ", paste(classes, collapse = ', '))
  }

  # Get a flat list if requested
  if (flat) {
    objs <- as.list(x, inherit = TRUE)
  } else {
    objs <- x
  }

  str <- paste0(
    "<gganimintproto object", classes_str(x), ">\n",
    indent(object_summaries(objs, flat = flat), 4)
  )

  if (flat && !is.null(x$super)) {
    str <- paste0(
      str, "\n",
      indent(
        paste0("super: ", " <gganimintproto object", classes_str(x$super), ">"),
        4
      )
    )
  }

  str
}

# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x, exclude = NULL, flat = TRUE) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- sort(names(x))
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  obj_names <- setdiff(obj_names, exclude)

  values <- vapply(obj_names, function(name) {
    obj <- x[[name]]
    if (is.function(obj)) "function"
    else if (is.gganimintproto(obj)) format(obj, flat = flat)
    else if (is.environment(obj)) "environment"
    else if (is.null(obj)) "NULL"
    else if (is.atomic(obj)) trim(paste(as.character(obj), collapse = " "))
    else paste(class(obj), collapse = ", ")
  }, FUN.VALUE = character(1))

  paste0(obj_names, ": ", values, sep = "", collapse = "\n")
}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(\\n|^)(?!$)",
    paste0("\\1", paste(rep(" ", indent), collapse = "")),
    str,
    perl = TRUE
  )
}

# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim <- function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, 56), "...")
  else str
}

#' @export
print.gganimintproto_method <- function(x, ...) {
  cat(format(x), sep = "")
}

#' @export
format.gganimintproto_method <- function(x, ...) {

  # Given a function, return a string from srcref if present. If not present,
  # paste the deparsed lines of code together.
  format_fun <- function(fn) {
    srcref <- attr(fn, "srcref", exact = TRUE)
    if (is.null(srcref))
      return(paste(format(fn), collapse = "\n"))

    paste(as.character(srcref), collapse = "\n")
  }

  x <- unclass(x)
  paste0(
    "<gganimintproto method>",
    "\n  <Wrapper function>\n    ", format_fun(x),
    "\n\n  <Inner function (f)>\n    ", format_fun(environment(x)$f)
  )
}

# proto2 TODO: better way of getting formals for self$draw
gganimintproto_formals <- function(x) formals(environment(x)$f)
