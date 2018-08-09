find_a_scale <- function(a_aes, x, env = parent.frame()) {
  type <- a_scale_type(x)
  candidates <- paste("a_scale", a_aes, type, sep = "_")

  for (a_scale in candidates) {
    a_scale_f <- find_global(a_scale, env, mode = "function")
    if (!is.null(a_scale_f))
      return(a_scale_f())
  }

  # Failure to find a scale is not an error because some "a_aesthetics" don't
  # need scales (e.g. group), and it allows others to extend ggplot2 with
  # their own aesthetics

  return(NULL)
}

# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("animint2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

# Determine default type of a scale
a_scale_type <- function(x) UseMethod("a_scale_type")

#' @export
a_scale_type.default <- function(x) {
  message("Don't know how to automatically pick scale for object of type ",
          paste(class(x), collapse = "/"), ". Defaulting to continuous.")
  "continuous"
}

#' @export
a_scale_type.AsIs <- function(x) "identity"

#' @export
a_scale_type.logical <- function(x) "discrete"

#' @export
a_scale_type.character <- function(x) "discrete"

#' @export
a_scale_type.ordered <- function(x) c("ordinal", "discrete")

#' @export
a_scale_type.factor <- function(x) "discrete"

#' @export
a_scale_type.POSIXt <- function(x) c("datetime", "continuous")

#' @export
a_scale_type.Date <- function(x) c("date", "continuous")

#' @export
a_scale_type.numeric <- function(x) "continuous"
