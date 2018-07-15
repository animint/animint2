#' Set guides for each scale.
#'
#' Guides for each scale can be set in call of \code{a_scale_*} with argument
#' \code{guide}, or in \code{guides}.
#'
#' @param ... List of scale guide pairs
#' @return A list containing the mapping between scale and guide.
#' @export
#' @family guides
#' @examples
#' \donttest{
#' # ggplot object
#'
#' dat <- data.frame(x = 1:5, y = 1:5, p = 1:5, q = factor(1:5),
#'  r = factor(1:5))
#' p <- a_plot(dat, aes(x, y, colour = p, size = q, shape = r)) + geom_point()
#'
#' # without guide specification
#' p
#'
#' # Show colorbar guide for colour.
#' # All these examples below have a same effect.
#'
#' p + guides(colour = "colorbar", size = "legend", shape = "legend")
#' p + guides(colour = guide_colorbar(), size = guide_legend(),
#'   shape = guide_legend())
#' p +
#'  a_scale_colour_continuous(guide = "colorbar") +
#'  a_scale_size_discrete(guide = "legend") +
#'  a_scale_shape(guide = "legend")
#'
#'  # Remove some guides
#'  p + guides(colour = "none")
#'  p + guides(colour = "colorbar",size = "none")
#'
#' # Guides are integrated where possible
#'
#' p + guides(colour = guide_legend("title"), size = guide_legend("title"),
#'   shape = guide_legend("title"))
#' # same as
#' g <- guide_legend("title")
#' p + guides(colour = g, size = g, shape = g)
#'
#' p + a_theme(legend.position = "bottom")
#'
#' # position of guides
#'
#' p + a_theme(legend.position = "bottom", legend.box = "horizontal")
#'
#' # Set order for multiple guides
#' a_plot(mpg, aes(displ, cty)) +
#'   geom_point(aes(size = hwy, colour = cyl, shape = drv)) +
#'   guides(
#'    colour = guide_colourbar(order = 1),
#'    shape = guide_legend(order = 2),
#'    size = guide_legend(order = 3)
#'  )
#' }
guides <- function(...) {
  args <- list(...)
  if (is.list(args[[1]]) && !inherits(args[[1]], "guide")) args <- args[[1]]
  args <- rename_aes(args)
  structure(args, class = "guides")
}

update_guides <- function(p, guides) {
  p <- a_plot_clone(p)
  p$guides <- defaults(guides, p$guides)
  p
}


# building guides - called in ggplotGrob (plot-render.r)
#
# the procedure is as follows:
#
# 1. guides_train()
#      train each scale and generate guide definition for all guides
#      here, one gdef for one scale
#
# 2. guides_merge()
#      merge gdefs if they are overlayed
#      number of gdefs may be less than number of scales
#
# 3. guides_geom()
#      process layer information and generate geom info.
#
# 4. guides_gengrob()
#      generate ggrob from each gdef
#      one ggrob for one gdef
#
# 5. guides_build()
#      arrange all ggrobs

build_guides <- function(scales, layers, default_mapping, position, a_theme, guides, labels) {

  # set a_themes w.r.t. guides
  # should these a_theme$legend.XXX be renamed to a_theme$guide.XXX ?

  # by default, guide boxes are vertically aligned
  a_theme$legend.box <- a_theme$legend.box %||% "vertical"

  # size of key (also used for bar in colorbar guide)
  a_theme$legend.key.width <- a_theme$legend.key.width %||% a_theme$legend.key.size
  a_theme$legend.key.height <- a_theme$legend.key.height %||% a_theme$legend.key.size

  # by default, direction of each guide depends on the position of the guide.
  a_theme$legend.direction <-
    a_theme$legend.direction %||%
    if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
      switch(position[1], top = , bottom = "horizontal", left = , right = "vertical")
    else
      "vertical"

  # justification of legend boxes
  a_theme$legend.box.just <-
    a_theme$legend.box.just %||%
    if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
      switch(position, bottom = , top = c("center", "top"), left = , right = c("left", "top"))
    else
      c("center", "center")

  # scales -> data for guides
  gdefs <- guides_train(scales = scales, a_theme = a_theme, guides = guides, labels = labels)
  if (length(gdefs) == 0) return(a_zeroGrob())

  # merge overlay guides
  gdefs <- guides_merge(gdefs)

  # process layer information
  gdefs <- guides_geom(gdefs, layers, default_mapping)
  if (length(gdefs) == 0) return(a_zeroGrob())

  # generate grob of each guides
  ggrobs <- guides_gengrob(gdefs, a_theme)

  # build up guides
  grobs <- guides_build(ggrobs, a_theme)

  grobs
}

#' validate_guide function
#'
#' @param guide ...
#' @export
validate_guide <- function(guide) {
  # if guide is specified by character, then find the corresponding guide
  if (is.character(guide))
    match.fun(paste("guide_", guide, sep = ""))()
  else if (inherits(guide, "guide"))
    guide
  else
    stop("Unknown guide: ", guide)
}

#' train each scale in scales and generate the definition of guide
#' @param scales ...
#' @param a_theme ...
#' @param guides ...
#' @param labels ....
#' @export
guides_train <- function(scales, a_theme, guides, labels) {

  gdefs <- list()
  for (a_scale in scales$scales) {

    # guides(XXX) is stored in guides[[XXX]],
    # which is prior to a_scale_ZZZ(guide=XXX)
    # guide is determined in order of:
    #   + guides(XXX) > + a_scale_ZZZ(guide=XXX) > default(i.e., legend)
    output <- a_scale$aesthetics[1]
    guide <- guides[[output]] %||% a_scale$guide

    # this should be changed to testing guide == "none"
    # a_scale$legend is backward compatibility
    # if guides(XXX=FALSE), then a_scale_ZZZ(guides=XXX) is discarded.
    if (guide == "none" || (is.logical(guide) && !guide)) next

    # check the validity of guide.
    # if guide is character, then find the guide object
    guide <- validate_guide(guide)

    # check the consistency of the guide and scale.
    if (guide$available_aes != "any" && !a_scale$aesthetics %in% guide$available_aes)
      stop("Guide '", guide$name, "' cannot be used for '", a_scale$aesthetics, "'.")

    guide$title <- guide$title %|W|% a_scale$name %|W|% labels[[output]]

    # direction of this grob
    guide$direction <- guide$direction %||% a_theme$legend.direction

    # each guide object trains a_scale within the object,
    # so Guides (i.e., the container of guides) need not to know about them
    guide <- guide_train(guide, a_scale)

    if (!is.null(guide)) gdefs[[length(gdefs) + 1]] <- guide
  }
  gdefs
}

#' merge overlapped guides
#' @param gdefs ...
#' @export
guides_merge <- function(gdefs) {
  # split gdefs based on hash, and apply Reduce (guide_merge) to each gdef group.
  gdefs <- lapply(gdefs, function(g) {
    if (g$order == 0) {
      order <- "99"
    } else {
      order <- sprintf("%02d", g$order)
    }
    g$hash <- paste(order, g$hash, sep = "_")
    g
  })
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(guide_merge, gs))
}

#' guides_geom function
#' @param gdefs ...
#' @param layers ....
#' @param default_mapping ...
#' @export
guides_geom <- function(gdefs, layers, default_mapping) {
  compact(lapply(gdefs, guide_geom, layers, default_mapping))
}

# generate grob from each gdef (needs to write this function?)
guides_gengrob <- function(gdefs, a_theme) {
  # common drawing process for all guides
  gdefs <- lapply(gdefs,
    function(g) {
      g$title.position <- g$title.position %||% switch(g$direction, vertical = "top", horizontal = "left")
      if (!g$title.position %in% c("top", "bottom", "left", "right"))
        stop("title position \"", g$title.position, "\" is invalid")
      g
    })

  lapply(gdefs, guide_gengrob, a_theme)
}

#' build up all guide boxes into one guide-boxes.
#' @param ggrobs ...
#' @param a_theme ...
#' @export
guides_build <- function(ggrobs, a_theme) {
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

    guides <- gtable_row(name = "guides",
      grobs = ggrobs,
      widths = widths, height = max(heights))

    # add space between the guide-boxes
    guides <- gtable_add_col_space(guides, a_theme$legend.hmargin)

  } else if (a_theme$legend.box == "vertical") {
    # Set justification for each legend
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]] <- editGrob(ggrobs[[i]],
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
          width = widthDetails(ggrobs[[i]])))
    }

    guides <- gtable_col(name = "guides",
      grobs = ggrobs,
      width = max(widths), heights = heights)

    # add space between the guide-boxes
    guides <- gtable_add_row_space(guides, a_theme$legend.vmargin)
  }

  # add margins around the guide-boxes.
  guides <- gtable_add_cols(guides, a_theme$legend.hmargin, pos = 0)
  guides <- gtable_add_cols(guides, a_theme$legend.hmargin, pos = ncol(guides))
  guides <- gtable_add_rows(guides, a_theme$legend.vmargin, pos = 0)
  guides <- gtable_add_rows(guides, a_theme$legend.vmargin, pos = nrow(guides))

  guides$name <- "guide-box"
  guides
}

# S3 dispatches

guide_train <- function(...) UseMethod("guide_train")

guide_merge <- function(...) UseMethod("guide_merge")

guide_geom <- function(...) UseMethod("guide_geom")

guide_gengrob <- function(...) UseMethod("guide_gengrob")
