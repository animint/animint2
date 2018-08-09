#' Set guides for each scale.
#'
#' Guides for each scale can be set in call of \code{a_scale_*} with argument
#' \code{a_guide}, or in \code{a_guides}.
#'
#' @param ... List of scale guide pairs
#' @return A list containing the mapping between scale and guide.
#' @export
#' @family a_guides
#' @examples
#' \donttest{
#' # ggplot object
#'
#' dat <- data.frame(x = 1:5, y = 1:5, p = 1:5, q = factor(1:5),
#'  r = factor(1:5))
#' p <- a_plot(dat, a_aes(x, y, colour = p, size = q, shape = r)) + a_geom_point()
#'
#' # without guide specification
#' p
#'
#' # Show colorbar guide for colour.
#' # All these examples below have a same effect.
#'
#' p + a_guides(colour = "colorbar", size = "legend", shape = "legend")
#' p + a_guides(colour = a_guide_colorbar(), size = a_guide_legend(),
#'   shape = a_guide_legend())
#' p +
#'  a_scale_colour_continuous(a_guide = "colorbar") +
#'  a_scale_size_discrete(a_guide = "legend") +
#'  a_scale_shape(a_guide = "legend")
#'
#'  # Remove some a_guides
#'  p + a_guides(colour = "none")
#'  p + a_guides(colour = "colorbar",size = "none")
#'
#' # Guides are integrated where possible
#'
#' p + a_guides(colour = a_guide_legend("title"), size = a_guide_legend("title"),
#'   shape = a_guide_legend("title"))
#' # same as
#' g <- a_guide_legend("title")
#' p + a_guides(colour = g, size = g, shape = g)
#'
#' p + a_theme(legend.a_position = "bottom")
#'
#' # a_position of a_guides
#'
#' p + a_theme(legend.a_position = "bottom", legend.box = "horizontal")
#'
#' # Set order for multiple guides
#' a_plot(mpg, a_aes(displ, cty)) +
#'   a_geom_point(a_aes(size = hwy, colour = cyl, shape = drv)) +
#'   a_guides(
#'    colour = a_guide_colourbar(order = 1),
#'    shape = a_guide_legend(order = 2),
#'    size = a_guide_legend(order = 3)
#'  )
#' }
a_guides <- function(...) {
  args <- list(...)
  if (is.list(args[[1]]) && !inherits(args[[1]], "a_guide")) args <- args[[1]]
  args <- rename_aes(args)
  structure(args, class = "a_guides")
}

update_a_guides <- function(p, a_guides) {
  p <- a_plot_clone(p)
  p$a_guides <- defaults(a_guides, p$a_guides)
  p
}


# building guides - called in ggplotGrob (plot-render.r)
#
# the procedure is as follows:
#
# 1. a_guides_train()
#      train each scale and generate guide definition for all guides
#      here, one gdef for one scale
#
# 2. a_guides_merge()
#      merge gdefs if they are overlayed
#      number of gdefs may be less than number of scales
#
# 3. a_guides_geom()
#      process layer information and generate geom info.
#
# 4. a_guides_gengrob()
#      generate ggrob from each gdef
#      one ggrob for one gdef
#
# 5. a_guides_build()
#      arrange all ggrobs

build_guides <- function(scales, layers, default_mapping, a_position, a_theme, a_guides, a_labels) {

  # set a_themes w.r.t. guides
  # should these a_theme$legend.XXX be renamed to a_theme$guide.XXX ?

  # by default, guide boxes are vertically aligned
  a_theme$legend.box <- a_theme$legend.box %||% "vertical"

  # size of key (also used for bar in colorbar guide)
  a_theme$legend.key.width <- a_theme$legend.key.width %||% a_theme$legend.key.size
  a_theme$legend.key.height <- a_theme$legend.key.height %||% a_theme$legend.key.size

  # by default, direction of each guide depends on the a_position of the guide.
  a_theme$legend.direction <-
    a_theme$legend.direction %||%
    if (length(a_position) == 1 && a_position %in% c("top", "bottom", "left", "right"))
      switch(a_position[1], top = , bottom = "horizontal", left = , right = "vertical")
    else
      "vertical"

  # justification of legend boxes
  a_theme$legend.box.just <-
    a_theme$legend.box.just %||%
    if (length(a_position) == 1 && a_position %in% c("top", "bottom", "left", "right"))
      switch(a_position, bottom = , top = c("center", "top"), left = , right = c("left", "top"))
    else
      c("center", "center")

  # scales -> data for guides
  gdefs <- a_guides_train(scales = scales, a_theme = a_theme, a_guides = a_guides, a_labels = a_labels)
  if (length(gdefs) == 0) return(a_zeroGrob())

  # merge overlay guides
  gdefs <- a_guides_merge(gdefs)

  # process layer information
  gdefs <- a_guides_geom(gdefs, layers, default_mapping)
  if (length(gdefs) == 0) return(a_zeroGrob())

  # generate grob of each a_guides
  ggrobs <- a_guides_gengrob(gdefs, a_theme)

  # build up a_guides
  grobs <- a_guides_build(ggrobs, a_theme)

  grobs
}

#' validate_guide function
#'
#' @param a_guide ...
#' @export
validate_guide <- function(a_guide) {
  # if guide is specified by character, then find the corresponding guide
  if (is.character(a_guide))
    match.fun(paste("a_guide_", a_guide, sep = ""))()
  else if (inherits(a_guide, "a_guide"))
    a_guide
  else
    stop("Unknown a_guide: ", a_guide)
}

#' train each scale in scales and generate the definition of guide
#' @param scales ...
#' @param a_theme ...
#' @param a_guides ...
#' @param a_labels ....
#' @export
a_guides_train <- function(scales, a_theme, a_guides, a_labels) {

  gdefs <- list()
  for (a_scale in scales$scales) {

    # a_guides(XXX) is stored in a_guides[[XXX]],
    # which is prior to a_scale_ZZZ(a_guide=XXX)
    # a_guide is determined in order of:
    #   + a_guides(XXX) > + a_scale_ZZZ(a_guide=XXX) > default(i.e., legend)
    output <- a_scale$a_aesthetics[1]
    a_guide <- a_guides[[output]] %||% a_scale$a_guide

    # this should be changed to testing a_guide == "none"
    # a_scale$legend is backward compatibility
    # if a_guides(XXX=FALSE), then a_scale_ZZZ(a_guides=XXX) is discarded.
    if (a_guide == "none" || (is.logical(a_guide) && !a_guide)) next

    # check the validity of a_guide.
    # if a_guide is character, then find the a_guide object
    a_guide <- validate_guide(a_guide)

    # check the consistency of the guide and scale.
    if (a_guide$available_aes != "any" && !a_scale$a_aesthetics %in% a_guide$available_aes)
      stop("Guide '", a_guide$name, "' cannot be used for '", a_scale$a_aesthetics, "'.")

    a_guide$title <- a_guide$title %|W|% a_scale$name %|W|% a_labels[[output]]

    # direction of this grob
    a_guide$direction <- a_guide$direction %||% a_theme$legend.direction

    # each guide object trains a_scale within the object,
    # so Guides (i.e., the container of guides) need not to know about them
    a_guide <- a_guide_train(a_guide, a_scale)

    if (!is.null(a_guide)) gdefs[[length(gdefs) + 1]] <- a_guide
  }
  gdefs
}

#' merge overlapped guides
#' @param gdefs ...
#' @export
a_guides_merge <- function(gdefs) {
  # split gdefs based on hash, and apply Reduce (a_guide_merge) to each gdef group.
  gdefs <- lapply(gdefs, function(g) {
    if (g$order == 0) {
      order <- "99"
    } else {
      order <- sprintf("%02d", g$order)
    }
    g$hash <- paste(order, g$hash, sep = "_")
    g
  })
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(a_guide_merge, gs))
}

#' a_guides_geom function
#' @param gdefs ...
#' @param layers ....
#' @param default_mapping ...
#' @export
a_guides_geom <- function(gdefs, layers, default_mapping) {
  compact(lapply(gdefs, a_guide_geom, layers, default_mapping))
}

# generate grob from each gdef (needs to write this function?)
a_guides_gengrob <- function(gdefs, a_theme) {
  # common drawing process for all guides
  gdefs <- lapply(gdefs,
    function(g) {
      g$title.a_position <- g$title.a_position %||% switch(g$direction, vertical = "top", horizontal = "left")
      if (!g$title.a_position %in% c("top", "bottom", "left", "right"))
        stop("title a_position \"", g$title.a_position, "\" is invalid")
      g
    })

  lapply(gdefs, a_guide_gengrob, a_theme)
}

#' build up all guide boxes into one guide-boxes.
#' @param ggrobs ...
#' @param a_theme ...
#' @export
a_guides_build <- function(ggrobs, a_theme) {
  a_theme$legend.margin <- a_theme$legend.margin %||% unit(0.5, "lines")
  a_theme$legend.vmargin <- a_theme$legend.vmargin  %||% a_theme$legend.margin
  a_theme$legend.hmargin <- a_theme$legend.hmargin  %||% a_theme$legend.margin

  widths <- do.call("unit.c", lapply(ggrobs, function(g)sum(g$widths)))
  heights <- do.call("unit.c", lapply(ggrobs, function(g)sum(g$heights)))

  # Set the justification of each legend within the legend box
  # First value is xjust, second value is yjust
  just <- valid.just(a_theme$legend.box.just)
  xjust <- just[1]
  yjust <- just[2]

  # setting that is different for vertical and horizontal guide-boxes.
  if (a_theme$legend.box == "horizontal") {
    # Set justification for each legend
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]] <- editGrob(ggrobs[[i]],
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
          height = heightDetails(ggrobs[[i]])))
    }

    a_guides <- gtable_row(name = "a_guides",
      grobs = ggrobs,
      widths = widths, height = max(heights))

    # add space between the guide-boxes
    a_guides <- gtable_add_col_space(a_guides, a_theme$legend.hmargin)

  } else if (a_theme$legend.box == "vertical") {
    # Set justification for each legend
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]] <- editGrob(ggrobs[[i]],
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
          width = widthDetails(ggrobs[[i]])))
    }

    a_guides <- gtable_col(name = "a_guides",
      grobs = ggrobs,
      width = max(widths), heights = heights)

    # add space between the guide-boxes
    a_guides <- gtable_add_row_space(a_guides, a_theme$legend.vmargin)
  }

  # add margins around the guide-boxes.
  a_guides <- gtable_add_cols(a_guides, a_theme$legend.hmargin, pos = 0)
  a_guides <- gtable_add_cols(a_guides, a_theme$legend.hmargin, pos = ncol(a_guides))
  a_guides <- gtable_add_rows(a_guides, a_theme$legend.vmargin, pos = 0)
  a_guides <- gtable_add_rows(a_guides, a_theme$legend.vmargin, pos = nrow(a_guides))

  a_guides$name <- "a_guide-box"
  a_guides
}

# S3 dispatches

a_guide_train <- function(...) UseMethod("a_guide_train")

a_guide_merge <- function(...) UseMethod("a_guide_merge")

a_guide_geom <- function(...) UseMethod("a_guide_geom")

a_guide_gengrob <- function(...) UseMethod("a_guide_gengrob")
