#' Get, set and update a_themes.
#'
#' Use \code{a_theme_get} to get the current a_theme, and \code{a_theme_set} to
#' completely override it. \code{a_theme_update} and \code{a_theme_replace} are
#' shorthands for changing individual elements in the current a_theme.
#' \code{a_theme_update} uses the \code{+} operator, so that any unspecified
#' values in the a_theme element will default to the values they are set in the
#' a_theme. \code{a_theme_replace} will completely replace the element, so any
#' unspecified values will overwrite the current value in the a_theme with \code{NULL}s.
#'
#'
#' @param ... named list of a_theme settings
#' @seealso \code{\link{\%+replace\%}} and \code{\link{+.aaa}}
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point()
#' p
#' old <- a_theme_set(a_theme_bw())
#' p
#' a_theme_set(old)
#' p
#'
#' #a_theme_replace NULLs out the fill attribute of panel.background,
#' #resulting in a white background:
#' a_theme_get()$panel.background
#' old <- a_theme_replace(panel.background = a_element_rect(colour = "pink"))
#' a_theme_get()$panel.background
#' p
#' a_theme_set(old)
#'
#' #a_theme_update only changes the colour attribute, leaving the others intact:
#' old <- a_theme_update(panel.background = a_element_rect(colour = "pink"))
#' a_theme_get()$panel.background
#' p
#' a_theme_set(old)
#'
#' a_theme_get()
#'
#'
#' a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point(a_aes(color = mpg)) +
#'   a_theme(legend.a_position = c(0.95, 0.95),
#'         legend.justification = c(1, 1))
#' last_plot() +
#'  a_theme(legend.background = a_element_rect(fill = "white", colour = "white", size = 3))
#'
a_theme_update <- function(...) {
  a_theme_set(a_theme_get() + a_theme(...))
}

#' @rdname a_theme_update
#' @export
a_theme_replace <- function(...) {
  a_theme_set(a_theme_get() %+replace% a_theme(...))
}

#' Reports whether x is a a_theme object
#' @param x An object to test
#' @export
is.a_theme <- function(x) inherits(x, "a_theme")

#' @export
print.a_theme <- function(x, ...) utils::str(x)

#' Set a_theme elements
#'
#'
#' Use this function to modify a_theme settings.
#'
#' a_theme elements can inherit properties from other a_theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title},
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#'
#' For more examples of modifying properties using inheritance, see
#' \code{\link{+.aaa}} and \code{\link{\%+replace\%}}.
#'
#' To see a graphical representation of the inheritance tree, see the
#' last example below.
#'
#' @section a_theme elements:
#' The individual a_theme elements are:
#'
#' \tabular{ll}{
#'   line             \tab all line elements
#'                    (\code{a_element_line}) \cr
#'   rect             \tab all rectangular a_elements
#'                    (\code{a_element_rect}) \cr
#'   text             \tab all text a_elements
#'                    (\code{a_element_text}) \cr
#'   title            \tab all title elements: plot, axes, legends
#'                    (\code{a_element_text}; inherits from \code{text}) \cr
#'   aspect.ratio     \tab aspect ratio of the panel \cr
#'
#'   axis.title       \tab a_label of axes
#'                    (\code{a_element_text}; inherits from \code{text}) \cr
#'   axis.title.x     \tab x axis a_label
#'                    (\code{a_element_text}; inherits from \code{axis.title}) \cr
#'   axis.title.y     \tab y axis a_label
#'                    (\code{a_element_text}; inherits from \code{axis.title}) \cr
#'   axis.text        \tab tick labels along axes
#'                    (\code{a_element_text}; inherits from \code{text}) \cr
#'   axis.text.x      \tab x axis tick labels
#'                    (\code{a_element_text}; inherits from \code{axis.text}) \cr
#'   axis.text.y      \tab y axis tick labels
#'                    (\code{a_element_text}; inherits from \code{axis.text}) \cr
#'   axis.ticks       \tab tick marks along axes
#'                    (\code{a_element_line}; inherits from \code{line}) \cr
#'   axis.ticks.x     \tab x axis tick marks
#'                    (\code{a_element_line}; inherits from \code{axis.ticks}) \cr
#'   axis.ticks.y     \tab y axis tick marks
#'                    (\code{a_element_line}; inherits from \code{axis.ticks}) \cr
#'   axis.ticks.length  \tab length of tick marks
#'                    (\code{unit}) \cr
#'   axis.line        \tab lines along axes
#'                    (\code{a_element_line}; inherits from \code{line}) \cr
#'   axis.line.x      \tab line along x axis
#'                    (\code{a_element_line}; inherits from \code{axis.line}) \cr
#'   axis.line.y      \tab line along y axis
#'                    (\code{a_element_line}; inherits from \code{axis.line}) \cr
#'
#'   legend.background  \tab background of legend
#'                    (\code{a_element_rect}; inherits from \code{rect}) \cr
#'   legend.margin    \tab extra space added around legend
#'                    (\code{unit}) \cr
#'   legend.key       \tab background underneath legend keys
#'                    (\code{a_element_rect}; inherits from \code{rect}) \cr
#'   legend.key.size  \tab size of legend keys
#'                    (\code{unit}; inherits from \code{legend.key.size}) \cr
#'   legend.key.height  \tab key background height
#'                    (\code{unit}; inherits from \code{legend.key.size}) \cr
#'   legend.key.width   \tab key background width
#'                    (\code{unit}; inherits from \code{legend.key.size}) \cr
#'   legend.text      \tab legend item labels
#'                    (\code{a_element_text}; inherits from \code{text}) \cr
#'   legend.text.align  \tab alignment of legend labels
#'                    (number from 0 (left) to 1 (right)) \cr
#'   legend.title     \tab title of legend
#'                    (\code{a_element_text}; inherits from \code{title}) \cr
#'   legend.title.align \tab alignment of legend title
#'                    (number from 0 (left) to 1 (right)) \cr
#'   legend.a_position  \tab the a_position of legends
#'                    ("none", "left", "right", "bottom", "top", or two-element
#'                      numeric vector) \cr
#'   legend.direction \tab layout of items in legends
#'                    ("horizontal" or "vertical") \cr
#'   legend.justification \tab anchor point for positioning legend inside plot
#'                    ("center" or two-element numeric vector) \cr
#'   legend.box       \tab arrangement of multiple legends
#'                    ("horizontal" or "vertical") \cr
#'   legend.box.just  \tab justification of each legend within the overall
#'                    bounding box, when there are multiple legends
#'                    ("top", "bottom", "left", or "right")\cr
#'
#'   panel.background \tab background of plotting area, drawn underneath plot
#'                    (\code{a_element_rect}; inherits from \code{rect}) \cr
#'   panel.border     \tab border around plotting area, drawn on top of plot
#'                    so that it covers tick marks and grid lines. This should
#'                    be used with \code{fill=NA}
#'                    (\code{a_element_rect}; inherits from \code{rect}) \cr
#'   panel.margin     \tab margin around facet panels
#'                    (\code{unit}) \cr
#'   panel.margin.x   \tab horizontal margin around facet panels
#'                    (\code{unit}; inherits from \code{panel.margin}) \cr
#'   panel.margin.y   \tab vertical margin around facet panels
#'                    (\code{unit}; inherits from \code{panel.margin}) \cr
#'   panel.grid       \tab grid lines
#'                    (\code{a_element_line}; inherits from \code{line}) \cr
#'   panel.grid.major \tab major grid lines
#'                    (\code{a_element_line}; inherits from \code{panel.grid}) \cr
#'   panel.grid.minor \tab minor grid lines
#'                    (\code{a_element_line}; inherits from \code{panel.grid}) \cr
#'   panel.grid.major.x \tab vertical major grid lines
#'                    (\code{a_element_line}; inherits from \code{panel.grid.major}) \cr
#'   panel.grid.major.y \tab horizontal major grid lines
#'                    (\code{a_element_line}; inherits from \code{panel.grid.major}) \cr
#'   panel.grid.minor.x \tab vertical minor grid lines
#'                    (\code{a_element_line}; inherits from \code{panel.grid.minor}) \cr
#'   panel.grid.minor.y \tab horizontal minor grid lines
#'                    (\code{a_element_line}; inherits from \code{panel.grid.minor}) \cr
#'   panel.ontop        \tab option to place the panel (background, gridlines)
#'                           over the data layers.  Usually used with a transparent
#'                           or blank \code{panel.background}. (\code{logical}) \cr
#'
#'   plot.background  \tab background of the entire plot
#'                    (\code{a_element_rect}; inherits from \code{rect}) \cr
#'   plot.title       \tab plot title (text appearance)
#'                    (\code{a_element_text}; inherits from \code{title})
#'                    left-aligned by default\cr
#'   plot.subtitle    \tab plot subtitle (text appearance)
#'                    (\code{a_element_text}; inherits from \code{title})
#'                    left-aligned by default\cr
#'   plot.caption     \tab caption below the plot (text appearance)
#'                    (\code{a_element_text}; inherits from \code{title})
#'                    right-aligned by default\cr
#'   plot.margin      \tab margin around entire plot
#'                    (\code{unit} with the sizes of the top, right, bottom, and
#'                     left margins) \cr
#'
#'   strip.background \tab background of facet labels
#'                    (\code{a_element_rect}; inherits from \code{rect}) \cr
#'   strip.text       \tab facet labels
#'                    (\code{a_element_text}; inherits from \code{text}) \cr
#'   strip.text.x     \tab facet labels along horizontal direction
#'                    (\code{a_element_text}; inherits from \code{strip.text}) \cr
#'   strip.text.y     \tab facet labels along vertical direction
#'                    (\code{a_element_text}; inherits from \code{strip.text}) \cr
#'   strip.switch.pad.grid \tab space between strips and axes when strips are switched
#'                    (\code{unit}) \cr
#'   strip.switch.pad.wrap \tab space between strips and axes when strips are switched
#'                    (\code{unit}) \cr
#' }
#'
#' @param ... a list of a_element name, element pairings that modify the
#'   existing a_theme.
#' @param complete set this to TRUE if this is a complete a_theme, such as
#'   the one returned \code{by a_theme_grey()}. Complete a_themes behave
#'   differently when added to a ggplot object.
#' @param validate TRUE to run validate_element, FALSE to bypass checks.
#'
#' @seealso \code{\link{+.aaa}}
#' @seealso \code{\link{\%+replace\%}}
#' @seealso \code{\link{rel}}
#' @seealso \code{\link{a_element_blank}}
#' @seealso \code{\link{a_element_line}}
#' @seealso \code{\link{a_element_rect}}
#' @seealso \code{\link{a_element_text}}
#' @export
#' @examples
#' \donttest{
#' p <- a_plot(mtcars, a_aes(mpg, wt)) +
#'   a_geom_point()
#' p
#' p + a_theme(panel.background = a_element_rect(colour = "pink"))
#' p + a_theme_bw()
#'
#' # Scatter plot of gas mileage by vehicle weight
#' p <- a_plot(mtcars, a_aes(wt, mpg)) +
#'   a_geom_point()
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + a_geom_abline(intercept = 37, slope = -5)
#' # Calculate correlation coefficient
#' with(mtcars, cor(wt, mpg, use = "everything", method = "pearson"))
#' #annotate the plot
#' p + a_geom_abline(intercept = 37, slope = -5) +
#' a_geom_text(data = data.frame(), a_aes(4.5, 30, a_label = "Pearson-R = -.87"))
#'
#' # Change the axis labels
#' # Original plot
#' p
#' p + labs(x = "Vehicle Weight", y = "Miles per Gallon")
#' # Or
#' p + labs(x = "Vehicle Weight", y = "Miles per Gallon")
#'
#' # Change title appearance
#' p <- p + labs(title = "Vehicle Weight-Gas Mileage Relationship")
#' # Set title to twice the base font size
#' p + a_theme(plot.title = a_element_text(size = rel(2)))
#' p + a_theme(plot.title = a_element_text(size = rel(2), colour = "blue"))
#'
#' # Add a subtitle and adjust bottom margin
#' p + labs(title = "Vehicle Weight-Gas Mileage Relationship",
#'          subtitle = "You need to wrap long subtitleson manually") +
#'     a_theme(plot.subtitle = a_element_text(margin = margin(b = 20)))
#'
#' # Changing plot look with a_themes
#' DF <- data.frame(x = rnorm(400))
#' m <- a_plot(DF, a_aes(x = x)) +
#'   a_geom_histogram()
#' # Default is a_theme_grey()
#' m
#' # Compare with
#' m + a_theme_bw()
#'
#' # Manipulate Axis Attributes
#' m + a_theme(axis.line = a_element_line(size = 3, colour = "red", linetype = "dotted"))
#' m + a_theme(axis.text = a_element_text(colour = "blue"))
#' m + a_theme(axis.text.y = a_element_blank())
#' m + a_theme(axis.ticks = a_element_line(size = 2))
#' m + a_theme(axis.title.y = a_element_text(size = rel(1.5), angle = 90))
#' m + a_theme(axis.title.x = a_element_blank())
#' m + a_theme(axis.ticks.length = unit(.85, "cm"))
#'
#' # Legend Attributes
#' z <- a_plot(mtcars, a_aes(wt, mpg)) +
#'   a_geom_point(a_aes(colour = factor(cyl)))
#' z
#' z + a_theme(legend.a_position = "none")
#' z + a_theme(legend.a_position = "bottom")
#' # Or use relative coordinates between 0 and 1
#' z + a_theme(legend.a_position = c(.5, .5))
#' # Add a border to the whole legend
#' z + a_theme(legend.background = a_element_rect(colour = "black"))
#' # Legend margin controls extra space around outside of legend:
#' z + a_theme(legend.background = a_element_rect(),
#'           legend.margin = unit(1, "cm"))
#' z + a_theme(legend.background = a_element_rect(),
#'           legend.margin = unit(0, "cm"))
#' # Or to just the keys
#' z + a_theme(legend.key = a_element_rect(colour = "black"))
#' z + a_theme(legend.key = a_element_rect(fill = "yellow"))
#' z + a_theme(legend.key.size = unit(2.5, "cm"))
#' z + a_theme(legend.text = a_element_text(size = 20, colour = "red", angle = 45))
#' z + a_theme(legend.title = a_element_text(face = "italic"))
#'
#' # To change the title of the legend use the name argument
#' # in one of the scale options
#' z + a_scale_colour_brewer(name = "My Legend")
#' z + a_scale_colour_grey(name = "Number of \nCylinders")
#'
#' # Panel and Plot Attributes
#' z + a_theme(panel.background = a_element_rect(fill = "black"))
#' z + a_theme(panel.border = a_element_rect(linetype = "dashed", colour = "black"))
#' z + a_theme(panel.grid.major = a_element_line(colour = "blue"))
#' z + a_theme(panel.grid.minor = a_element_line(colour = "red", linetype = "dotted"))
#' z + a_theme(panel.grid.major = a_element_line(size = 2))
#' z + a_theme(panel.grid.major.y = a_element_blank(),
#'           panel.grid.minor.y = a_element_blank())
#' z + a_theme(plot.background = a_element_rect())
#' z + a_theme(plot.background = a_element_rect(fill = "green"))
#'
#' # Faceting Attributes
#' set.seed(4940)
#' dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
#' k <- a_plot(dsmall, a_aes(carat, ..density..)) +
#'   a_geom_histogram(binwidth = 0.2) +
#'   a_facet_grid(. ~ cut)
#' k + a_theme(strip.background = a_element_rect(colour = "purple", fill = "pink",
#'                                           size = 3, linetype = "dashed"))
#' k + a_theme(strip.text.x = a_element_text(colour = "red", angle = 45, size = 10,
#'                                       hjust = 0.5, vjust = 0.5))
#' k + a_theme(panel.margin = unit(5, "lines"))
#' k + a_theme(panel.margin.y = unit(0, "lines"))
#'
#' # Put gridlines on top
#' meanprice <- tapply(diamonds$price, diamonds$cut, mean)
#' cut <- factor(levels(diamonds$cut), levels = levels(diamonds$cut))
#' df <- data.frame(meanprice, cut)
#' g <- a_plot(df, a_aes(cut, meanprice)) + a_geom_bar(a_stat = "identity")
#' g + a_geom_bar(a_stat = "identity") +
#'     a_theme(panel.background = a_element_blank(),
#'           panel.grid.major.x = a_element_blank(),
#'           panel.grid.minor.x = a_element_blank(),
#'           panel.grid.minor.y = a_element_blank(),
#'           panel.ontop = TRUE)
#'
#' # Modify a a_theme and save it
#' mya_theme <- a_theme_grey() + a_theme(plot.title = a_element_text(colour = "red"))
#' p + mya_theme
#'
#' }
a_theme <- function(..., complete = FALSE, validate = TRUE) {
  elements <- list(...)

  if (!is.null(elements$axis.ticks.margin)) {
    warning("`axis.ticks.margin` is deprecated. Please set `margin` property ",
      " of `axis.text` instead", call. = FALSE)
    elements$axis.ticks.margin <- NULL
  }

  # Check that all elements have the correct class (a_element_text, unit, etc)
  if (validate) {
    mapply(validate_element, elements, names(elements))
  }

  structure(elements, class = c("a_theme", "aaa"),
            complete = complete, validate = validate)
}

#' Combine plot defaults with current a_theme to get complete a_theme for a plot
#' @param x ....
#' @export
plot_a_theme <- function(x) {
  defaults(x$a_theme, a_theme_get())
}


.a_theme <- (function() {
  a_theme <- a_theme_gray()

  list(
    get = function() a_theme,
    set = function(new) {
      missing <- setdiff(names(a_theme_gray()), names(new))
      if (length(missing) > 0) {
        warning("New a_theme missing the following elements: ",
          paste(missing, collapse = ", "), call. = FALSE)
      }

      old <- a_theme
      a_theme <<- new
      invisible(old)
    }
  )
})()


#' @rdname a_theme_update
#' @export
a_theme_get <- .a_theme$get
#' @rdname a_theme_update
#' @param new new a_theme (a list of a_theme elements)
#' @export
a_theme_set <- .a_theme$set


#' @rdname aaa-add
#' @export
"%+replace%" <- function(e1, e2) {
  if (!is.a_theme(e1) || !is.a_theme(e2)) {
    stop("%+replace% requires two a_theme objects", call. = FALSE)
  }

  # Can't use modifyList here since it works recursively and drops NULLs
  e1[names(e2)] <- e2
  e1
}


#' Modify properties of an element in a a_theme object
#'
#' @param t1 A a_theme object
#' @param t2 A a_theme object that is to be added to \code{t1}
#' @param t2name A name of the t2 object. This is used for printing
#'   informative error messages.
#'
#' @seealso +.aaa
#'
add_a_theme <- function(t1, t2, t2name) {
  if (!is.a_theme(t2)) {
    stop("Don't know how to add ", t2name, " to a a_theme object",
      call. = FALSE)
  }

  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]

    if (is.null(x) || inherits(x, "a_element_blank")) {
      # If x is NULL or element_blank, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y) ||
               is.logical(y) || inherits(y, "a_element_blank")) {
      # If y is NULL, or a string or numeric vector, or is element_blank, just replace x
      x <- y
    } else {
      # If x is not NULL, then copy over the non-NULL properties from y
      # Get logical vector of non-NULL properties in y
      idx <- !vapply(y, is.null, logical(1))
      # Get the names of TRUE items
      idx <- names(idx[idx])

      # Update non-NULL items
      x[idx] <- y[idx]
    }

    # Assign it back to t1
    # This is like doing t1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    t1[item] <- list(x)
  }

  # If either a_theme is complete, then the combined a_theme is complete
  attr(t1, "complete") <- attr(t1, "complete") || attr(t2, "complete")
  t1
}


#' Update a a_theme from a plot object
#'
#' This is called from add_a_plot.
#'
#' If newa_theme is a *complete* a_theme, then it is meant to replace
#' olda_theme; this function just returns newa_theme.
#'
#' Otherwise, it adds elements from newa_theme to olda_theme:
#' If olda_theme doesn't already contain those elements,
#' it searches the current default a_theme, grabs the elements with the
#' same name as those from newa_theme, and puts them in olda_theme. Then
#' it adds elements from newa_theme to olda_theme.
#' This makes it possible to do things like:
#'   a_plot(data.frame(x = 1:3, y = 1:3)) +
#'   a_geom_point() + a_theme(text = a_element_text(colour = 'red'))
#' and have 'text' keep properties from the default a_theme. Otherwise
#' you would have to set all the element properties, like family, size,
#' etc.
#'
#' @param olda_theme an existing a_theme, usually from a plot object, like
#'   plot$a_theme. This could be an empty list.
#' @param newa_theme a new a_theme object to add to the existing a_theme
#' @export
update_a_theme <- function(olda_theme, newa_theme) {
  # If the newa_theme is a complete one, don't bother searching
  # the default a_theme -- just replace everything with newa_theme
  if (attr(newa_theme, "complete"))
    return(newa_theme)

  # These are elements in newa_theme that aren't already set in olda_theme.
  # They will be pulled from the default a_theme.
  newitems <- !names(newa_theme) %in% names(olda_theme)
  newitem_names <- names(newa_theme)[newitems]
  olda_theme[newitem_names] <- a_theme_get()[newitem_names]

  # Update the a_theme elements with the things from newa_theme
  # Turn the 'a_theme' list into a proper a_theme object first, and preserve
  # the 'complete' attribute. It's possible that olda_theme is an empty
  # list, and in that case, set complete to FALSE.
  old.validate <- isTRUE(attr(olda_theme, "validate"))
  new.validate <- isTRUE(attr(newa_theme, "validate"))
  olda_theme <- do.call(a_theme, c(olda_theme,
    complete = isTRUE(attr(olda_theme, "complete")),
    validate = old.validate & new.validate))

  olda_theme + newa_theme
}

#' Calculate the element properties, by inheriting properties from its parents
#'
#' @param a_element The name of the a_theme element to calculate
#' @param a_theme A a_theme object (like a_theme_grey())
#' @param verbose If TRUE, print out which elements this one inherits from
#' @examples
#' t <- a_theme_grey()
#' calc_element('text', t)
#'
#' # Compare the "raw" element definition to the element with calculated inheritance
#' t$axis.text.x
#' calc_element('axis.text.x', t, verbose = TRUE)
#'
#' # This reports that axis.text.x inherits from axis.text,
#' # which inherits from text. You can view each of them with:
#' t$axis.text.x
#' t$axis.text
#' t$text
#'
#' @export
calc_element <- function(a_element, a_theme, verbose = FALSE) {
  if (verbose) message(a_element, " --> ", appendLF = FALSE)

  # If this is a_element_blank, don't inherit anything from parents
  if (inherits(a_theme[[a_element]], "a_element_blank")) {
    if (verbose) message("a_element_blank (no inheritance)")
    return(a_theme[[a_element]])
  }

  # If the a_element is defined (and not just inherited), check that
  # it is of the class specified in .a_element_tree
  if (!is.null(a_theme[[a_element]]) &&
      !inherits(a_theme[[a_element]], .a_element_tree[[a_element]]$class)) {
    stop(a_element, " should have class ", .a_element_tree[[a_element]]$class)
  }

  # Get the names of parents from the inheritance tree
  pnames <- .a_element_tree[[a_element]]$inherit

  # If no parents, this is a "root" node. Just return this a_element.
  if (is.null(pnames)) {
    # Check that all the properties of this a_element are non-NULL
    nullprops <- vapply(a_theme[[a_element]], is.null, logical(1))
    if (any(nullprops)) {
      stop("a_theme element '", a_element, "' has NULL property: ",
        paste(names(nullprops)[nullprops], collapse = ", "))
    }

    if (verbose) message("nothing (top level)")
    return(a_theme[[a_element]])
  }

  # Calculate the parent objects' inheritance
  if (verbose) message(paste(pnames, collapse = ", "))
  parents <- lapply(pnames, calc_element, a_theme, verbose)

  # Combine the properties of this element with all parents
  Reduce(combine_elements, parents, a_theme[[a_element]])
}


# Combine the properties of two elements
#
# @param e1 An element object
# @param e2 An element object which e1 inherits from
combine_elements <- function(e1, e2) {

  # If e2 is NULL, nothing to inherit
  if (is.null(e2))  return(e1)

  # If e1 is NULL, or if e2 is a_element_blank, inherit everything from e2
  if (is.null(e1) || inherits(e2, "a_element_blank"))  return(e2)

  # If e1 has any NULL properties, inherit them from e2
  n <- vapply(e1[names(e2)], is.null, logical(1))
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  e1
}
