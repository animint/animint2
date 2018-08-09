#' Quick plot
#'
#' \code{qplot} is the basic plotting function in the ggplot2 package,
#' designed to be familiar if you're used to base \code{\link{plot}()}.
#' It's a convenient wrapper for creating a number of different types of plots
#' using a consistent calling scheme.
#'
#' @param x,y,... Aesthetics passed into each layer
#' @param data Data frame to use (optional).  If not specified, will create
#'   one, extracting vectors from the current environment.
#' @param facets faceting formula to use. Picks \code{\link{a_facet_wrap}} or
#'   \code{\link{a_facet_grid}} depending on whether the formula is one-
#'   or two-sided
#' @param margins See \code{a_facet_grid}: display marginal facets?
#' @param a_geom Character vector specifying geom(s) to draw. Defaults to
#'  "point" if x and y are specified, and "histogram" if only x is specified.
#' @param a_stat,a_position DEPRECATED.
#' @param xlim,ylim X and y axis limits
#' @param log Which variables to log transform ("x", "y", or "xy")
#' @param main,xlab,ylab Character vector (or expression) giving plot title,
#'   x axis label, and y axis label respectively.
#' @param asp The y/x aspect ratio
#' @export
#' @examples
#' # Use data from data.frame
#' qplot(mpg, wt, data = mtcars)
#' qplot(mpg, wt, data = mtcars, colour = cyl)
#' qplot(mpg, wt, data = mtcars, size = cyl)
#' qplot(mpg, wt, data = mtcars, facets = vs ~ am)
#'
#' \donttest{
#' qplot(1:10, rnorm(10), colour = runif(10))
#' qplot(1:10, letters[1:10])
#' mod <- lm(mpg ~ wt, data = mtcars)
#' qplot(resid(mod), fitted(mod))
#'
#' f <- function() {
#'    a <- 1:10
#'    b <- a ^ 2
#'    qplot(a, b)
#' }
#' f()
#'
#' # To set aesthetics, wrap in I()
#' qplot(mpg, wt, data = mtcars, colour = I("red"))
#'
#' # qplot will attempt to guess what geom you want depending on the input
#' # both x and y supplied = scatterplot
#' qplot(mpg, wt, data = mtcars)
#' # just x supplied = histogram
#' qplot(mpg, data = mtcars)
#' # just y supplied = scatterplot, with x = seq_along(y)
#' qplot(y = mpg, data = mtcars)
#'
#' # Use different geoms
#' qplot(mpg, wt, data = mtcars, a_geom = "path")
#' qplot(factor(cyl), wt, data = mtcars, a_geom = c("boxplot", "jitter"))
#' qplot(mpg, data = mtcars, a_geom = "dotplot")
#' }
qplot <- function(x, y = NULL, ..., data, facets = NULL, margins = FALSE,
                  a_geom = "auto", xlim = c(NA, NA),
                  ylim = c(NA, NA), log = "", main = NULL,
                  xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
                  asp = NA, a_stat = NULL, a_position = NULL) {

  if (!missing(a_stat)) warning("`stat` is deprecated", call. = FALSE)
  if (!missing(a_position)) warning("`position` is deprecated", call. = FALSE)
  if (!is.character(a_geom)) stop("`geom` must be a character vector", call. = FALSE)

  argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  env <- parent.frame()

  a_aesthetics <- compact(arguments[.all_a_aesthetics])
  a_aesthetics <- a_aesthetics[!is.constant(a_aesthetics)]
  a_aes_names <- names(a_aesthetics)
  a_aesthetics <- rename_aes(a_aesthetics)
  class(a_aesthetics) <- "uneval"

  if (missing(data)) {
    # If data not explicitly specified, will be pulled from workspace
    data <- data.frame()

    # Faceting variables must be in a data frame, so pull those out
    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    names(facetvars) <- facetvars
    facetsdf <- as.data.frame(mget(facetvars, envir = env))
    if (nrow(facetsdf)) data <- facetsdf
  }

  # Work out plot data, and modify aesthetics, if necessary
  if ("auto" %in% a_geom) {
    if ("sample" %in% a_aes_names) {
      a_geom[a_geom == "auto"] <- "qq"
    } else if (missing(y)) {
      x <- eval(a_aesthetics$x, data, env)
      if (is.discrete(x)) {
        a_geom[a_geom == "auto"] <- "bar"
      } else {
        a_geom[a_geom == "auto"] <- "histogram"
      }
      if (missing(ylab)) ylab <- "count"
    } else {
      if (missing(x)) {
        a_aesthetics$x <- bquote(seq_along(.(y)), a_aesthetics)
      }
      a_geom[a_geom == "auto"] <- "point"
    }
  }

  p <- a_plot(data, a_aesthetics, environment = env)

  if (is.null(facets)) {
    p <- p + a_facet_null()
  } else if (is.formula(facets) && length(facets) == 2) {
    p <- p + a_facet_wrap(facets)
  } else {
    p <- p + a_facet_grid(facets = deparse(facets), margins = margins)
  }

  if (!is.null(main)) p <- p + ggtitle(main)

  # Add geoms/statistics
  for (g in a_geom) {
    # Arguments are unevaluated because some are aesthetics. Need to evaluate
    # params - can't do in correct env because that's lost (no lazyeval)
    # so do the best we can by evaluating in parent frame.
    params <- arguments[setdiff(names(arguments), c(a_aes_names, argnames))]
    params <- lapply(params, eval, parent.frame())

    p <- p + do.call(paste0("a_geom_", g), params)
  }

  logv <- function(var) var %in% strsplit(log, "")[[1]]

  if (logv("x")) p <- p + a_scale_x_log10()
  if (logv("y")) p <- p + a_scale_y_log10()

  if (!is.na(asp)) p <- p + a_theme(aspect.ratio = asp)

  if (!missing(xlab)) p <- p + xlab(xlab)
  if (!missing(ylab)) p <- p + ylab(ylab)

  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)

  p
}

#' @export
#' @rdname qplot
quickplot <- qplot

is.constant <- function(x) {
  is_I_call <- function(x) is.call(x) && identical(x[[1]], quote(I))
  vapply(x, is_I_call, logical(1))
}
