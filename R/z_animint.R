#' Convert a ggplot to a list.
#' @param plot ...
#' @param plot.name ...
#' @param meta environment with previously calculated plot data, and a new plot to parse, already stored in plot and plot.name.
#' @return nothing, info is stored in meta.
#' @export
#' @import plyr
parsePlot <- function(meta, plot, plot.name){
  ## adding data and mapping to each layer from base plot, if necessary
  for(a_layer.i in seq_along(plot$layers)) {
    
    ## if data is not specified, get it from plot
    if(length(plot$layers[[a_layer.i]]$data) == 0){
      plot$layers[[a_layer.i]]$data <- plot$data
    }
    
    ## if mapping is not specified, get it from plot
    if(is.null(plot$layers[[a_layer.i]]$mapping)){
      plot$layers[[a_layer.i]]$mapping <- plot$mapping
    }
  }
  
  built <- a_plot_build(plot)
  plot.info <- list()
  
  ## Export axis specification as a combination of breaks and
  ## labels, on the relevant axis scale (i.e. so that it can
  ## be passed into d3 on the x axis scale instead of on the
  ## grid 0-1 scale). This allows transformations to be used
  ## out of the box, with no additional d3 coding.
  a_theme.pars <- plot_a_theme(plot)

  ## Interpret panel.margin as the number of lines between facets
  ## (ignoring whatever grid::unit such as cm that was specified).
  
  ## Now ggplot specifies panel.margin in 'pt' instead of 'lines'
  plot.info$panel_margin_lines <- pt.to.lines(a_theme.pars$panel.margin)
  
  ## No legend if a_theme(legend.postion="none").
  plot.info$legend <- if(a_theme.pars$legend.a_position != "none"){
    getLegendList(built)
  }
  
  ## scan for legends in each layer.
  for(a_layer.i in seq_along(plot$layers)){
    ##cat(sprintf("%4d / %4d layers\n", a_layer.i, length(plot$layers)))
    ## This is the a_layer from the original ggplot object.
    L <- plot$layers[[a_layer.i]]
    
    ## Use original mapping saved before calling parsePlot
    ## This is to handle cases where the plots may share the same layer
    ## If the layer mapping in one plot has been edited by the animint
    ## compiler, the layer mapping in the other plots will also change
    ## which will give error when we check if showSelected/clickSelects have
    ## been used as aesthetics
    L$mapping <- L$orig_mapping
    
    ## If any legends are specified, add showSelected aesthetic
    L <- addShowSelectedForLegend(meta, plot.info$legend, L)
    
    ## Check if showSelected and clickSelects have been used as aesthetics
    ## If yes, raise error
    checkForSSandCSasAesthetics(L$mapping, plot.name)
    
    ## Handle the extra_params argument
    ## -> handles .value/.variable named params
    ## -> removes duplicates
    ## -> removes duplicates due to showSelected legend
    L$extra_params <- checkExtraParams(L$extra_params, L$mapping)
    
    ## Add the showSelected/clickSelects params to the aesthetics
    ## mapping before calling a_plot_build
    L$mapping <- addSSandCSasAesthetics(L$mapping, L$extra_params)
  }#layer.i

  ## need to call ggplot_build again because we've added to the plot.
  ## I'm sure that there is a way around this, but not immediately sure how. 
  ## There's sort of a Catch-22 here because to create the interactivity, 
  ## we need to specify the variable corresponding to each legend. 
  ## To do this, we need to have the legend. 
  ## And to have the legend, I think that we need to use a_plot_build
  built <- a_plot_build(plot)
  ## TODO: implement a compiler that does not call a_plot_build at
  ## all, and instead does all of the relevant computations in animint
  ## code.
  ## 'strips' are really titles for the different facet panels
  plot.info$strips <- with(built, getStrips(plot$a_facet, panel))
  
  ## the layout tells us how to subset and where to plot on the JS side
  plot.info$layout <- with(built, flag_axis(plot$a_facet, panel$layout))
  plot.info$layout <- with(built, train_layout(
    plot$a_facet, plot$coordinates, plot.info$layout, panel$ranges))
  
  # saving background info
  plot.info$panel_background <- get_bg(a_theme.pars$panel.background, a_theme.pars)
  plot.info$panel_border <- get_bg(a_theme.pars$panel.border, a_theme.pars)
  
  # extract major grid lines
  plot.info$grid_major <- get_grid(a_theme.pars$panel.grid.major, a_theme.pars,
                                   plot.info, meta, built)
  # extract minor grid lines
  plot.info$grid_minor <- get_grid(a_theme.pars$panel.grid.minor, a_theme.pars,
                                   plot.info, meta, built, major = F)
  
  ## Flip labels if coords are flipped - transform does not take care
  ## of this. Do this BEFORE checking if it is blank or not, so that
  ## individual axes can be hidden appropriately, e.g. #1.
  if("a_CoordFlip"%in%attr(plot$coordinates, "class")){
    temp <- plot$a_labels$x
    plot$a_labels$x <- plot$a_labels$y
    plot$a_labels$y <- temp
  }
  is.blank <- function(el.name){
    x <- calc_element(el.name, plot$a_theme)
    "a_element_blank"%in%attr(x,"class")
  }

  # Instead of an "axis" JSON object for each plot,
  # allow for "axis1", "axis2", etc. where
  # "axis1" corresponds to the 1st PANEL
  ranges <- built$panel$ranges
  n.axis <- length(ranges)
  axes <- setNames(vector("list", n.axis),
                   paste0("axis", seq_len(n.axis)))
  plot.info <- c(plot.info, axes)

  # translate axis information
  for (xy in c("x", "y")) {
    s <- function(tmp) sprintf(tmp, xy)
    # one axis name per plot (ie, a xtitle/ytitle is shared across panels)
    plot.info[[s("%stitle")]] <- if(is.blank(s("axis.title.%s"))){
      ""
    } else {
      a_scale.i <- which(plot$scales$find(xy))
      lab.or.null <- if(length(a_scale.i) == 1){
        plot$scales$scales[[a_scale.i]]$name
      }
      if(is.null(unlist(lab.or.null))){
        plot$a_labels[[xy]]
      }else{
        lab.or.null
      }
    }
    # a_theme settings are shared across panels
    axis.text <- a_theme.pars[[s("axis.text.%s")]]
    ## TODO: also look at axis.text! (and text?)
    anchor <- hjust2anchor(axis.text$hjust)
    angle <- if(is.numeric(axis.text$angle)){
      -axis.text$angle
    }
    if(is.null(angle)){
      angle <- 0
    }
    if(is.null(anchor)){
      anchor <- if(angle == 0){
        "middle"
      }else{
        "end"
      }
    }
    plot.info[[s("%sanchor")]] <- as.character(anchor)
    plot.info[[s("%sangle")]] <- as.numeric(angle)
    # translate panel specific axis info
    ctr <- 0
    for (axis in names(axes)) {
      ctr <- ctr + 1
      range <- ranges[[ctr]]
      plot.info[[axis]][[xy]] <- as.list(range[[s("%s.major_source")]])
      plot.info[[axis]][[s("%slab")]] <- if(is.blank(s("axis.text.%s"))){
        NULL
      } else {
        as.list(range[[s("%s.a_labels")]])
      }
      plot.info[[axis]][[s("%srange")]] <- range[[s("%s.range")]]
      plot.info[[axis]][[s("%sline")]] <- !is.blank(s("axis.line.%s"))
      plot.info[[axis]][[s("%sticks")]] <- !is.blank(s("axis.ticks.%s"))
    }
  }
  # grab the unique axis labels (makes rendering simpler)
  plot.info <- getUniqueAxisLabels(plot.info)

  # grab plot title if present
  plot.info$title <- getPlotTitle(a_theme.pars$plot.tiltle,
                                  plot$a_labels$title)

  ## Set plot width and height from animint.* options if they are
  ## present.
  options_list <- getWidthAndHeight(plot$a_theme)
  options_list <- setUpdateAxes(plot$a_theme, options_list)
  plot.info$options <- options_list

  list(
    plot.info=plot.info,
    a_plot=plot,
    built=built)
}


storeLayer <- function(meta, g, g.data.varied){
  ## Save each variable chunk to a separate tsv file.
  meta$chunk.i <- 1L
  meta$g <- g
  g$chunks <- saveChunks(g.data.varied, meta)
  g$total <- length(unlist(g$chunks))
  
  ## Finally save to the master geom list.
  meta$geoms[[g$classed]] <- g
  g
}

#' Save a layer to disk, save and return meta-data.
#' @param l one layer of the ggplot object.
#' @param d one layer of calculated data from a_plot_build(p).
#' @param meta environment of meta-data.
#' @param geom_num the number of geom in the plot. Each geom gets an increasing
#' @param a_layer_name .....
#' @param a_plot ....
#' @param built ....
#' @param AniminationInfo
#'  ID number starting from 1
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
saveLayer <- function(l, d, meta, a_layer_name, a_plot, built, AnimationInfo){
  # Set geom name and layer name
  # Now that a_layer_name have become geom_a_position like we need to spilit with [3] index
  # ToDO: write a if statement with warning message if correct geom name
  # and a_layer names are not passed
  g <- list(a_geom=strsplit(a_layer_name, "_")[[1]][3])
  g$classed <- a_layer_name
  
  ranges <- built$panel$ranges
  
  ## needed for when group, etc. is an expression:
  g$a_aes <- sapply(l$mapping, function(k) as.character(as.expression(k)))

  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  ## 'colour', 'size' etc. have been moved to a_aes_params
  g$params <- getLayerParams(l)
  
  ## Make a list of variables to use for subsetting. subset_order is the
  ## order in which these variables will be accessed in the recursive
  ## JavaScript array structure.

  ## subset_order IS in fact useful with a_geom_segment! For example, in
  ## the first plot in the breakpointError example, the a_geom_segment has
  ## the following exported data in plot.json

  ## "subset_order": [
  ##  "showSelected",
  ## "showSelected2"
  ## ],

  ## This information is used to parse the recursive array data structure
  ## that allows efficient lookup of subsets of data in JavaScript. Look at
  ## the Firebug DOM browser on
  ## http://sugiyama-www.cs.titech.ac.jp/~toby/animint/breakpoints/index.html
  ## and navigate to plot.Geoms.geom3.data. You will see that this is a
  ## recursive array that can be accessed via
  ## data[segments][bases.per.probe] which is an un-named array
  ## e.g. [{row1},{row2},...] which will be bound to the <line> elements by
  ## D3. The key point is that the subset_order array stores the order of the
  ## indices that will be used to select the current subset of data (in
  ## this case showSelected=segments, showSelected2=bases.per.probe). The
  ## currently selected values of these variables are stored in
  ## plot.Selectors.

  ## Separate .variable/.value selectors
  s.a_aes <- selectSSandCS(g$a_aes)
  meta$selector.a_aes[[g$classed]] <- s.a_aes

  ## Do not copy group unless it is specified in a_aes, and do not copy
  ## showSelected variables which are specified multiple times.
  do.not.copy <- colsNotToCopy(g, s.a_aes)
  copy.cols <- ! names(d) %in% do.not.copy
  
  g.data <- d[copy.cols]
  
  is.ss <- names(g$a_aes) %in% s.a_aes$showSelected$one
  show.vars <- g$a_aes[is.ss]
  pre.subset.order <- as.list(names(show.vars))

  is.cs <- names(g$a_aes) %in% s.a_aes$clickSelects$one
  update.vars <- g$a_aes[is.ss | is.cs]

  update.var.names <- if(0 < length(update.vars)){
    data.frame(variable=names(update.vars), value=NA)
  }
  
  interactive.a_aes <- with(s.a_aes, {
    rbind(clickSelects$several, showSelected$several,
          update.var.names)
  })

  ## Construct the selector.
  for(row.i in seq_along(interactive.a_aes$variable)){
    a_aes.row <- interactive.a_aes[row.i, ]
    is.variable.value <- !is.na(a_aes.row$value)
    selector.df <- if(is.variable.value){
      selector.vec <- g.data[[paste(a_aes.row$variable)]]
      data.frame(value.col=a_aes.row$value,
                 selector.name=unique(paste(selector.vec)))
    }else{
      value.col <- paste(a_aes.row$variable)
      data.frame(value.col,
                 selector.name=update.vars[[value.col]])
    }
    for(sel.i in 1:nrow(selector.df)){
      sel.row <- selector.df[sel.i,]
      value.col <- paste(sel.row$value.col)
      selector.name <- paste(sel.row$selector.name)
      ## If this selector was defined by .variable .value a_aes, then we
      ## will not generate selectize widgets.
      meta$selectors[[selector.name]]$is.variable.value <- is.variable.value
      ## If this selector has no defined type yet, we define it once
      ## and for all here, so we can use it later for chunk
      ## separation.
      if(is.null(meta$selectors[[selector.name]]$type)){
        selector.type <- meta$selector.types[[selector.name]]
        if(is.null(selector.type))selector.type <- "single"
        stopifnot(is.character(selector.type))
        stopifnot(length(selector.type)==1)
        stopifnot(selector.type %in% c("single", "multiple"))
        meta$selectors[[selector.name]]$type <- selector.type
      }
      ## If this selector does not have any clickSelects then we show
      ## the selectize widgets by default.
      for(look.for in c("showSelected", "clickSelects")){
        if(grepl(look.for, a_aes.row$variable)){
          meta$selectors[[selector.name]][[look.for]] <- TRUE
        }
      }
      ## We also store all the values of this selector in this layer,
      ## so we can accurately set levels after all geoms have been
      ## compiled.
      value.vec <- unique(g.data[[value.col]])
      key <- paste(g$classed, row.i, sel.i)
      meta$selector.values[[selector.name]][[key]] <-
        list(values=paste(value.vec), update=g$classed)
    }
  }

  is.show <- grepl("showSelected", names(g$a_aes))
  has.show <- any(is.show)
  ## Error if non-identity stat is used with showSelected, since
  ## typically the stats will delete the showSelected column from the
  ## built data set. For example a_geom_bar + a_stat_bin doesn't make
  ## sense with clickSelects/showSelected, since two
  ## clickSelects/showSelected values may show up in the same bin.
  a_stat.type <- class(l$a_stat)[[1]]
  checkForNonIdentityAndSS(a_stat.type, has.show, is.show, l,
                           g$classed, names(g.data), names(g$a_aes))
  
  ## Warn if non-identity position is used with animint a_aes.
  a_position.type <- class(l$a_position)[[1]]
  if(has.show && a_position.type != "a_PositionIdentity"){
    print(l)
    warning("showSelected only works with a_position=identity, problem: ",
            g$classed)
  }

  ##print("before pre-processing")

  ## Pre-process some complex geoms so that they are treated as
  ## special cases of basic geoms. In ggplot2, this processing is done
  ## in the draw method of the geoms.
  if(g$a_geom=="abline"){
    ## loop through each set of slopes/intercepts
    
    ## TODO: vectorize this code!
    for(i in 1:nrow(g.data)) {
      
      # "Trick" ggplot coord_transform into transforming the slope and intercept
      g.data[i, "x"] <- ranges[[ g.data$PANEL[i] ]]$x.range[1]
      g.data[i, "xend"] <- ranges[[ g.data$PANEL[i] ]]$x.range[2]
      g.data[i, "y"] <- g.data$slope[i] * g.data$x[i] + g.data$intercept[i]
      g.data[i, "yend"] <- g.data$slope[i] * g.data$xend[i] + g.data$intercept[i]
      
      # make sure that lines don't run off the graph
      if(g.data$y[i] < ranges[[ g.data$PANEL[i] ]]$y.range[1] ) {
        g.data$y[i] <- ranges[[ g.data$PANEL[i] ]]$y.range[1]
        g.data$x[i] <- (g.data$y[i] - g.data$intercept[i]) / g.data$slope[i]
      }
      if(g.data$yend[i] > ranges[[ g.data$PANEL[i] ]]$y.range[2]) {
        g.data$yend[i] <- ranges[[ g.data$PANEL[i] ]]$y.range[2]
        g.data$xend[i] <- (g.data$yend[i] - g.data$intercept[i]) / g.data$slope[i]
      }
    }
    ## ggplot2 defaults to adding a group a_aes for ablines!
    ## Remove it since it is meaningless.
    g$a_aes <- g$a_aes[names(g$a_aes)!="group"]
    g.data <- g.data[! names(g.data) %in% c("slope", "intercept")]
    g$a_geom <- "segment"
  } else if(g$a_geom=="point"){
    # Fill set to match ggplot2 default of filled in circle.
    # Check for fill in both data and params
    fill.in.data <- ("fill" %in% names(g.data) && any(!is.na(g.data[["fill"]])))
    fill.in.params <- "fill" %in% names(g$params)
    fill.specified <- fill.in.data || fill.in.params
    if(!fill.specified & "colour" %in% names(g.data)){
      g.data[["fill"]] <- g.data[["colour"]]
    }
  } else if(g$a_geom=="text"){
    ## convert hjust to anchor.
    hjustRemove <- function(df.or.list){
      df.or.list$anchor <- hjust2anchor(df.or.list$hjust)
      df.or.list[names(df.or.list) != "hjust"]
    }
    vjustWarning <- function(vjust.vec){
      not.supported <- vjust.vec != 0
      if(any(not.supported)){
        bad.vjust <- unique(vjust.vec[not.supported])
        print(bad.vjust)
        warning("animint only supports vjust=0")
      }
    }
    if ("hjust" %in% names(g$params)) {
      g$params <- hjustRemove(g$params)
    } else if ("hjust" %in% names(g.data)) {
      g.data <- hjustRemove(g.data)
    } 
    if("vjust" %in% names(g$params)) {
      vjustWarning(g$params$vjust)
    } else if ("vjust" %in% names(g$a_aes)) {
      vjustWarning(g.data$vjust)
    } 
  } else if(g$a_geom=="ribbon"){
    # Color set to match ggplot2 default of fill with no outside border.
    if("fill"%in%names(g.data) & !"colour"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
    }
  } else if(g$a_geom=="density" | g$a_geom=="area"){
    g$a_geom <- "ribbon"
  } else if(g$a_geom=="tile" | g$a_geom=="raster" | g$a_geom=="histogram" ){
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g.data) & "fill"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g.data)) g.data[["size"]] <- 0
    }
    g$a_geom <- "rect"
  } else if(g$a_geom=="bar"){
    is.xy <- names(g.data) %in% c("x", "y")
    g.data <- g.data[!is.xy]
    g$a_geom <- "rect"
  } else if(g$a_geom=="bin2d"){
    stop("bin2d is not supported in animint. Try using a_geom_tile() and binning the data yourself.")
  } else if(g$a_geom=="boxplot"){
    stop("boxplots are not supported. Workaround: rects, lines, and points")
    ## TODO: boxplot support. But it is hard since boxplots are drawn
    ## using multiple geoms and it is not straightforward to deal with
    ## that using our current JS code. There is a straightforward
    ## workaround: combine working geoms (rects, lines, and points).

    g.data$outliers <- sapply(g.data$outliers, FUN=paste, collapse=" @ ")
    # outliers are specified as a list... change so that they are specified
    # as a single string which can then be parsed in JavaScript.
    # there has got to be a better way to do this!!
  } else if(g$a_geom=="violin"){
    g.data$xminv <- with(g.data, x - violinwidth * (x - xmin))
    g.data$xmaxv <- with(g.data, x + violinwidth * (xmax - x))
    newdata <- plyr::ddply(g.data, "group", function(df){
                  rbind(plyr::arrange(transform(df, x=xminv), y), 
                        plyr::arrange(transform(df, x=xmaxv), -y))
                })
    newdata <- plyr::ddply(newdata, "group", function(df) rbind(df, df[1,]))
    g.data <- newdata
    g$a_geom <- "polygon"
  } else if(g$a_geom=="step"){
    datanames <- names(g.data)
    g.data <- plyr::ddply(g.data, "group", function(df) stairstep(df))
    g$a_geom <- "path"
  } else if(g$a_geom=="contour" | g$a_geom=="density2d"){
    g$a_aes[["group"]] <- "piece"
    g$a_geom <- "path"
  } else if(g$a_geom=="freqpoly"){
    g$a_geom <- "line"
  } else if(g$a_geom=="quantile"){
    g$a_geom <- "path"
  } else if(g$a_geom=="hex"){
    g$a_geom <- "polygon"
    ## TODO: for interactivity we will run into the same problems as
    ## we did with histograms. Again, if we put several
    ## clickSelects/showSelected values in the same hexbin, then
    ## clicking/hiding hexbins doesn't really make sense. Need to stop
    ## with an error if showSelected/clickSelects is used with hex.
    g$a_aes[["group"]] <- "group"
    dx <- resolution(g.data$x, FALSE)
    dy <- resolution(g.data$y, FALSE) / sqrt(3) / 2 * 1.15
    hex <- as.data.frame(hexbin::hexcoords(dx, dy))[,1:2]
    hex <- rbind(hex, hex[1,]) # to join hexagon back to first point
    g.data$group <- as.numeric(interaction(g.data$group, 1:nrow(g.data)))
    ## this has the potential to be a bad assumption -
    ##   by default, group is identically 1, if the user
    ##   specifies group, polygons aren't possible to plot
    ##   using d3, because group will have a different meaning
    ##   than "one single polygon".
    # CPS (07-24-14) what about this? --
    # http://tdhock.github.io/animint/geoms/polygon/index.html
    newdata <- plyr::ddply(g.data, "group", function(df){
      df$xcenter <- df$x
      df$ycenter <- df$y
      cbind(x=df$x+hex$x, y=df$y+hex$y, df[,-which(names(df)%in%c("x", "y"))])
    })
    g.data <- newdata
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g.data) & "fill"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g.data)) g.data[["size"]] <- 0
    }
  }

  ## Some geoms need their data sorted before saving to tsv.
  if(g$a_geom %in% c("ribbon", "line")){
    g.data <- g.data[order(g.data$x), ]
  }

  ## Check g.data for color/fill - convert to hexadecimal so JS can parse correctly.
  for(color.var in c("colour", "color", "fill")){
    if(color.var %in% names(g.data)){
      g.data[,color.var] <- toRGB(g.data[,color.var])
    }
    if(color.var %in% names(g$params)){
      g$params[[color.var]] <- toRGB(g$params[[color.var]])
    }
  }

  has.no.fill <- g$a_geom %in% c("path", "line")
  zero.size <- any(g.data$size == 0, na.rm=TRUE)
  if(zero.size && has.no.fill){
    warning(sprintf("a_geom_%s with size=0 will be invisible",g$a_geom))
  }
  ## TODO: a_coord_transform maybe won't work for
  ## a_geom_dotplot|rect|segment and polar/log transformations, which
  ## could result in something nonlinear. For the time being it is
  ## best to just ignore this, but you can look at the source of
  ## e.g. geom-rect.r in ggplot2 to see how they deal with this by
  ## doing a piecewise linear interpolation of the shape.
  
  ## Flip axes in case of coord_flip
  if(inherits(a_plot$coordinates, "a_CoordFlip")){
    names(g.data) <- switch_axes(names(g.data))
  }

  ## Output types
  ## Check to see if character type is d3's rgb type.
  g$types <- sapply(g.data, function(x) {
    type <- paste(class(x), collapse="-")
    if(type == "character"){
      if(sum(!is.rgb(x))==0){
        "rgb"
      }else if(sum(!is.linetype(x))==0){
        "linetype"
      }else {
        "character"
      }
    }else{
      type
    }
  })
  g$types[["group"]] <- "character"

  ## convert ordered factors to unordered factors so javascript
  ## doesn't flip out.
  ordfactidx <- which(g$types=="ordered-factor")
  for(i in ordfactidx){
    g.data[[i]] <- factor(as.character(g.data[[i]]))
    g$types[[i]] <- "factor"
  }

  ## Get unique values of time variable.
  time.col <- NULL
  if(is.list(AnimationInfo$time)){ # if this is an animation,
    g.time.list <- list()
    for(c.or.s in names(s.a_aes)){
      cs.info <- s.a_aes[[c.or.s]]
      for(a in cs.info$one){
        if(g$a_aes[[a]] == AnimationInfo$time$var){
          g.time.list[[a]] <- g.data[[a]]
          time.col <- a
        }
      }
      for(row.i in seq_along(cs.info$several$value)){
        cs.row <- cs.info$several[row.i,]
        c.name <- paste(cs.row$variable)
        is.time <- g.data[[c.name]] == AnimationInfo$time$var
        g.time.list[[c.name]] <- g.data[is.time, paste(cs.row$value)]
      }
    }
    u.vals <- unique(unlist(g.time.list))
    if(length(u.vals)){
      AnimationInfo$timeValues[[paste(g$classed)]] <- sort(u.vals)
    }
  }
  ## Make the time variable the first subset_order variable.
  if(length(time.col)){
    pre.subset.order <- pre.subset.order[order(pre.subset.order != time.col)]
  }

  ## Determine which showSelected values to use for breaking the data
  ## into chunks. This is a list of variables which have the same
  ## names as the selectors. E.g. if chunk_order=list("year") then
  ## when year is clicked, we may need to download some new data for
  ## this geom.
  subset.vec <- unlist(pre.subset.order)
  if("chunk_vars" %in% names(g$params)){ #designer-specified chunk vars.
    designer.chunks <- g$params$chunk_vars
    if(!is.character(designer.chunks)){
      stop("chunk_vars must be a character vector; ",
           "use chunk_vars=character() to specify 1 chunk")
    }
    not.subset <- !designer.chunks %in% g$a_aes[subset.vec]
    if(any(not.subset)){
      stop("invalid chunk_vars ",
           paste(designer.chunks[not.subset], collapse=" "),
           "; possible showSelected variables: ",
           paste(g$a_aes[subset.vec], collapse=" "))
    }
    is.chunk <- g$a_aes[subset.vec] %in% designer.chunks
    chunk.cols <- subset.vec[is.chunk]
    nest.cols <- subset.vec[!is.chunk]
  }else{ #infer a default, either 0 or 1 chunk vars:
    if(length(meta$selectors)==0){
      ## no selectors, just make 1 chunk.
      nest.cols <- subset.vec
      chunk.cols <- NULL
    }else{
      selector.types <- sapply(meta$selectors, "[[", "type")
      selector.names <- g$a_aes[subset.vec]
      subset.types <- selector.types[selector.names]
      can.chunk <- subset.types != "multiple"
      names(can.chunk) <- subset.vec
      ## Guess how big the chunk files will be, and reduce the number of
      ## chunks if there are any that are too small.
      tmp <- tempfile()
      some.lines <- rbind(head(g.data), tail(g.data))
      write.table(some.lines, tmp,
                  col.names=FALSE,
                  quote=FALSE, row.names=FALSE, sep="\t")
      bytes <- file.info(tmp)$size
      bytes.per.line <- bytes/nrow(some.lines)
      bad.chunk <- function(){
        if(all(!can.chunk))return(NULL)
        can.chunk.cols <- subset.vec[can.chunk]
        maybe.factors <- g.data[, can.chunk.cols, drop=FALSE]
        for(N in names(maybe.factors)){
          maybe.factors[[N]] <- paste(maybe.factors[[N]])
        }
        rows.per.chunk <- table(maybe.factors)
        bytes.per.chunk <- rows.per.chunk * bytes.per.line
        if(all(4096 < bytes.per.chunk))return(NULL)
        ## If all of the tsv chunk files are greater than 4KB, then we
        ## return NULL here to indicate that the current chunk
        ## variables (indicated in can.chunk) are fine.

        ## In other words, the compiler will not break a geom into
        ## chunks if any of the resulting chunk tsv files is estimated
        ## to be less than 4KB (of course, if the layer has very few
        ## data overall, the compiler creates 1 file which may be less
        ## than 4KB, but that is fine).
        dim.byte.list <- list()
        if(length(can.chunk.cols) == 1){
          dim.byte.list[[can.chunk.cols]] <- sum(bytes.per.chunk)
        }else{
          for(dim.i in seq_along(can.chunk.cols)){
            dim.name <- can.chunk.cols[[dim.i]]
            dim.byte.list[[dim.name]] <-
              apply(bytes.per.chunk, -dim.i, sum)
          }
        }
        selector.df <-
          data.frame(chunks.for=length(rows.per.chunk),
                     chunks.without=sapply(dim.byte.list, length),
                     min.bytes=sapply(dim.byte.list, min))
        ## chunks.for is the number of chunks you get if you split the
        ## data set using just this column. If it is 1, then it is
        ## fine to chunk on this variable (since we certainly won't
        ## make more than 1 small tsv file) and in fact we want to
        ## chunk on this variable, since then this layer's data won't
        ## be downloaded at first if it is not needed.
        not.one <- subset(selector.df, 1 < chunks.for)
        if(nrow(not.one) == 0){
          NULL
        }else{
          rownames(not.one)[[which.max(not.one$min.bytes)]]
        }
      }
      while({
        bad <- bad.chunk()
        !is.null(bad)
      }){
        can.chunk[[bad]] <- FALSE
      }
      if(any(can.chunk)){
        nest.cols <- subset.vec[!can.chunk]
        chunk.cols <- subset.vec[can.chunk]
      }else{
        nest.cols <- subset.vec
        chunk.cols <- NULL
      }
    } # meta$selectors > 0
  }
  
  # If there is only one PANEL, we don't need it anymore.
  # g$PANEL <- unique(g.data[["PANEL"]])
  plot.has.panels <- nrow(built$panel$layout) > 1
  g.data <- removeUniquePanelValue(g.data, plot.has.panels)
    
  ## Also add pointers to these chunks from the related selectors.
  if(length(chunk.cols)){
    selector.names <- as.character(g$a_aes[chunk.cols])
    chunk.name <- paste(selector.names, collapse="_")
    g$chunk_order <- as.list(selector.names)
    for(selector.name in selector.names){
      meta$selectors[[selector.name]]$chunks <-
        unique(c(meta$selectors[[selector.name]]$chunks, chunk.name))
    }
  }else{
    g$chunk_order <- list()
  }
  g$nest_order <- as.list(nest.cols)
  names(g$chunk_order) <- NULL
  names(g$nest_order) <- NULL
  g$subset_order <- g$nest_order
  
  ## If this plot has more than one PANEL then add it to subset_order
  ## and nest_order.
  if(plot.has.panels){
    g$subset_order <- c(g$subset_order, "PANEL")
    g$nest_order <- c(g$nest_order, "PANEL")
  }

  ## nest_order should contain both .variable .value a_aesthetics, but
  ## subset_order should contain only .variable.
  if((nrow(s.a_aes$showSelected$several) > 0)){
    g$nest_order <- with(s.a_aes$showSelected$several, {
      c(g$nest_order, paste(variable), paste(value))
    })
    g$subset_order <-
      c(g$subset_order, paste(s.a_aes$showSelected$several$variable))
  }
    
  ## group should be the last thing in nest_order, if it is present.
  data.object.geoms <- c("line", "path", "ribbon", "polygon")
  if("group" %in% names(g$a_aes) && g$a_geom %in% data.object.geoms){
    g$nest_order <- c(g$nest_order, "group")
  }

  ## Some geoms should be split into separate groups if there are NAs.
  if(any(is.na(g.data)) && "group" %in% names(g$a_aes)){
    sp.cols <- unlist(c(chunk.cols, g$nest_order))
    order.args <- list()
    for(sp.col in sp.cols){
      order.args[[sp.col]] <- g.data[[sp.col]]
    }
    ord <- do.call(order, order.args)
    g.data <- g.data[ord,]
    is.missing <- apply(is.na(g.data), 1, any)
    diff.vec <- diff(is.missing)
    new.group.vec <- c(FALSE, diff.vec == 1)
    for(chunk.col in sp.cols){
      one.col <- g.data[[chunk.col]]
      is.diff <- c(FALSE, one.col[-1] != one.col[-length(one.col)])
      new.group.vec[is.diff] <- TRUE
    }
    subgroup.vec <- cumsum(new.group.vec)
    g.data$group <- subgroup.vec
  }

  ## Determine if there are any "common" data that can be saved
  ## separately to reduce disk usage.
  data.or.null <- getCommonChunk(g.data, chunk.cols, g$a_aes)
  g.data.varied <- if(is.null(data.or.null)){
    split.x(na.omit(g.data), chunk.cols)
  }else{
    g$columns$common <- as.list(names(data.or.null$common))
    tsv.name <- sprintf("%s_chunk_common.tsv", g$classed)
    tsv.path <- file.path(meta$out.dir, tsv.name)
    write.table(data.or.null$common, tsv.path,
                quote = FALSE, row.names = FALSE, 
                sep = "\t")
    data.or.null$varied
  }

  list(g=g, g.data.varied=g.data.varied, timeValues=AnimationInfo$timeValues)
}


#' Compile and render an animint in a local directory
#'
#' An animint is a list of ggplots and options that defines
#' an interactive animation and can be viewed in a web browser.
#' Several new aesthetics control interactivity.
#' The most important two are
#' \itemize{
#' \item \code{a_aes(showSelected=variable)} means that
#'   only the subset of the data that corresponds to
#'   the selected value of variable will be shown.
#' \item \code{a_aes(clickSelects=variable)} means that clicking
#'   this geom will change the currently selected value of variable.
#' }
#' The others are described on https://github.com/tdhock/animint/wiki/Advanced-features-present-animint-but-not-in-ggplot2
#'
#' Supported ggplot2 geoms:
#' \itemize{
#' \item point
#' \item jitter
#' \item line
#' \item rect
#' \item tallrect (new with this package)
#' \item segment
#' \item hline
#' \item vline
#' \item bar
#' \item text
#' \item tile
#' \item raster
#' \item ribbon
#' \item abline
#' \item density
#' \item path
#' \item polygon
#' \item histogram
#' \item violin
#' \item linerange
#' \item step
#' \item contour
#' \item density2d
#' \item area
#' \item freqpoly
#' \item hex
#' }
#' Unsupported geoms:
#' \itemize{
#' \item rug
#' \item dotplot
#' \item quantile - should *theoretically* work but in practice does not work
#' \item smooth - can be created using a_geom_line and a_geom_ribbon
#' \item boxplot - can be created using a_geom_rect and a_geom_segment
#' \item crossbar - can be created using a_geom_rect and a_geom_segment
#' \item pointrange - can be created using a_geom_linerange and a_geom_point
#' \item bin2d - bin using ddply() and then use a_geom_tile()
#' \item map - can be created using a_geom_polygon or a_geom_path
#'}
#' Supported scales:
#' \itemize{
#' \item alpha,
#' \item fill/colour (brewer, gradient, identity, manual)
#' \item linetype
#' \item x and y axis scales, manual break specification, a_label formatting
#' \item x and y axis a_theme elements: axis.line, axis.ticks, axis.text, axis.title can be set to a_element_blank(); other a_theme modifications not supported at this time, but would be possible with custom css files.
#' \item area
#' \item size
#' }
#' Unsupported scales:
#' \itemize{
#' \item shape. Open and closed circles can be represented by manipulating fill and colour scales and using default (circle) points, but d3 does not support many R shape types, so mapping between the two is difficult.
#' }
#'
#' @aliases animint
#' @param plot.list a named list of ggplots and option lists.
#' @param out.dir directory to store html/js/csv files.
#' @param json.file character string that names the JSON file with metadata associated with the plot.
#' @param open.browser Should R open a browser? If yes, be sure to configure your browser to allow access to local files, as some browsers block this by default (e.g. chrome).
#' @param css.file character string for non-empty css file to include. Provided file will be copied to the output directory as styles.css
#' @return invisible list of ggplots in list format.
#' @export
#' @example inst/examples/animint.R
animint2dir <- function(plot.list, out.dir = tempfile(),
                        json.file = "plot.json", open.browser = interactive(),
                        css.file = "") {
  ## Check plot.list for errors
  checkPlotList(plot.list)
  
  ## Store meta-data in this environment, so we can alter state in the
  ## lower-level functions.
  meta <- newEnvironment()
  meta$selector.types <- plot.list$selector.types
  dir.create(out.dir,showWarnings=FALSE)
  meta$out.dir <- out.dir
  
  ## Store the animation information (time, var, ms) in a separate list
  AnimationInfo <- list()
  
  ## Save the animation variable so we can treat it specially when we
  ## process each geom.
  # CPS (7-22-14): What if the user doesn't specify milliseconds? Could we provide a reasonable default?
  if(is.list(plot.list[["time"]])){
    # Check animation variable for errors
    checkAnimationTimeVar(plot.list$time)
    AnimationInfo$time <- plot.list$time
    ms <- AnimationInfo$time$ms
    time.var <- AnimationInfo$time$variable
  }

  ## The title option should just be a character, not a list.
  if(is.list(plot.list$title)){
    plot.list$title <- plot.list$title[[1]]
  }
  if(is.character(plot.list$title)){
    meta$title <- plot.list$title[[1]]
    plot.list$title <- NULL
  }

  ## Extract essential info from ggplots, reality checks.
  for(list.name in names(plot.list)){
    p <- plot.list[[list.name]]
    if(is.a_plot(p)){
      ## Save original mapping to every layer so that we can use it afterwards
      ## This is required because we edit the original plot list created for the
      ## animint2dir function. See the discussion here:
      ## https://github.com/tdhock/animint2/pull/5#issuecomment-323072502
      for(a_layer_i in seq_along(p$layers)){
        ## Viz has not been used before
        if(is.null(p$layers[[a_layer_i]]$orig_mapping)){
          p$layers[[a_layer_i]]$orig_mapping <- 
            if(is.null(p$layers[[a_layer_i]]$mapping)){
              ## Get mapping from plot if not defined in layer
              p$mapping
            }else{
              p$layers[[a_layer_i]]$mapping
            }
        }else{
          ## This is not the first time this layer is being processed, so we replace
          ## the mapping with the original mapping here
          p$layers[[a_layer_i]]$mapping <- p$layers[[a_layer_i]]$orig_mapping
        }
      }
      
      ## Before calling a_plot_build, we do some error checking for
      ## some animint extensions.
      checkPlotForAnimintExtensions(p, list.name)
    }else if(is.list(p)){ ## for options.
      meta[[list.name]] <- p
    }else{
      stop("list items must be ggplots or option lists, problem: ", list.name)
    }
  }
  
  ## Call a_plot_build in parsPlot for all ggplots
  a_plot.list <- list()
  AllPlotsInfo <- list()
  for(list.name in names(plot.list)){
    p <- plot.list[[list.name]]
    if(is.a_plot(p)){
      ## If plot is correct, save to meta for further processing
      parsed_info <- parsePlot(meta, p, list.name) # calls a_plot_build.
      AllPlotsInfo[[list.name]] <- parsed_info$plot.info
      a_plot.list[[list.name]]$a_plot <- parsed_info$a_plot
      a_plot.list[[list.name]]$built <- parsed_info$built
    }
  }
  
  ## After going through all of the meta-data in all of the ggplots,
  ## now we have enough info to save the TSV file database.
  geom_num <- 0
  g.list <- list()
  for(p.name in names(a_plot.list)){
    a_plot.info <- a_plot.list[[p.name]]
    for(a_layer.i in seq_along(a_plot.info$a_plot$layers)){
      L <- a_plot.info$a_plot$layers[[a_layer.i]]
      df <- a_plot.info$built$data[[a_layer.i]]
      
      ## cat(sprintf(
      ##   "saving a_layer %4d / %4d of a_plot %s\n",
      ##   a_layer.i, length(a_plot.info$built$data),
      ##   p.name))
      
      ## Data now contains columns with fill, alpha, colour etc.
      ## Remove from data if they have a single unique value and
      ## are NOT used in mapping to reduce tsv file size
      redundant.cols <- names(L$a_geom$default_aes)
      for(col.name in names(df)){
        if(col.name %in% redundant.cols){
          all.vals <- unique(df[[col.name]])
          if(length(all.vals) == 1){
            in.mapping <-
              !is.null(L$mapping[[col.name]])
            if(!in.mapping){
              df[[col.name]] <- NULL
            }
          }
        }
      }
      geom_num <- geom_num + 1
      a_layer_name <- getLayerName(L, geom_num, p.name)
      gl <- saveLayer(L, df, meta, a_layer_name,
                      a_plot.info$a_plot, a_plot.info$built, AnimationInfo)
      
      ## Save Animation Info separately
      AnimationInfo$timeValues <- gl$timeValues
      gl$timeValues <- NULL
      ## Save to a list before saving to tsv
      ## Helps during axis updates and Inf values
      g.list[[p.name]][[gl$g$classed]] <- gl
    }#a_layer.i
  }
  
  ## Selector levels and update were stored in saveLayer, so now
  ## compute the unique values to store in meta$selectors.
  for(selector.name in names(meta$selector.values)){
    values.update <- meta$selector.values[[selector.name]]
    value.vec <- unique(unlist(lapply(values.update, "[[", "values")))
    meta$selectors[[selector.name]]$selected <- if(
      meta$selectors[[selector.name]]$type=="single"){
      value.vec[1]
    }else{
      value.vec
    }
    ## Check the selectize option to determine if the designer wants
    ## to show a widget for this selector.
    selectize <- meta$selectize[[selector.name]]
    render.widget <- if(is.logical(selectize)){
      selectize[1]
    }else{
      ## If the designer did not set selectize, then we set a default
      ## (if .variable .value a_aes, then no selectize; otherwise if
      ## there are less than 1000 values then yes).
      if(isTRUE(meta$selectors[[selector.name]]$is.variable.value)){
        FALSE
      }else{
        if(length(value.vec) < 1000){
          TRUE
        }else{
          FALSE
        }
      }
    }
    if(render.widget){
      ## Showing selectize widgets is optional, and indicated to the
      ## renderer by the compiler by not setting the "levels"
      ## attribute of the selector.
      meta$selectors[[selector.name]]$levels <- value.vec
    }
    ## s.info$update is the list of geom names that will be updated
    ## for this selector.
    meta$selectors[[selector.name]]$update <-
      as.list(unique(unlist(lapply(values.update, "[[", "update"))))
  }
  
  ## For a static data viz with no interactive a_aes, no need to check
  ## for trivial showSelected variables with only 1 level.
  checkSingleShowSelectedValue(meta$selectors)

  ## Go through options and add to the list.
  for(v.name in names(meta$duration)){
    meta$selectors[[v.name]]$duration <- meta$duration[[v.name]]
  }
  ## Set plot sizes.
  setPlotSizes(meta, AllPlotsInfo)
  
  ## Get domains of data subsets if a_theme_animint(update_axes) is used
  for(p.name in names(a_plot.list)){
    axes_to_update <- AllPlotsInfo[[p.name]]$options$update_axes
    if(!is.null(axes_to_update)){
      for (axis in axes_to_update){
        subset_domains <- list()
        # Determine if every panel needs a different domain or not
        # We conclude here if we want to split the data by PANEL
        # for the axes updates. Else every panel uses the same domain
        panels <- a_plot.list[[p.name]]$built$panel$layout$PANEL
        axes_drawn <- 
          a_plot.list[[p.name]]$built$panel$layout[[paste0("AXIS_",
                                                           toupper(axis))]]
        panels_used <- panels[axes_drawn]
        split_by_panel <- all(panels == panels_used)
        for(num in seq_along(a_plot.list[[p.name]]$built$plot$layers)){
          # If there is a geom where the axes updates have non numeric values,
          # we stop and throw an informative warning
          # It does not make sense to have axes updates for non numeric values
          a_aesthetic_names <- names(g.list[[p.name]][[num]]$g$a_aes)
          
          axis_col_name <- a_aesthetic_names[grepl(axis, a_aesthetic_names)]
          axis_col <- g.list[[p.name]][[num]]$g$a_aes[[ axis_col_name[[1]] ]]
          axis_is_numeric <- is.numeric(a_plot.list[[p.name]]$built$plot$layers[[num]]$data[[axis_col]])
          if(!axis_is_numeric){
            stop(paste0("'update_axes' specified for '", toupper(axis),
                        "' axis on plot '", p.name, 
                        "' but the column '", axis_col, "' is non-numeric.",
                        " Axes updates are only available for numeric data."))
          }
          
          # handle cases for showSelected: showSelectedlegendfill,
          # showSelectedlegendcolour etc.
          choose_ss <- grepl("^showSelected", a_aesthetic_names)
          ss_selectors <- g.list[[p.name]][[num]]$g$a_aes[choose_ss]
          # Do not calculate domains for multiple selectors
          remove_ss <- c()
          for(j in seq_along(ss_selectors)){
            if(meta$selectors[[ ss_selectors[[j]] ]]$type != "single"){
              remove_ss <- c(remove_ss, ss_selectors[j])
            }
          }
          ss_selectors <- ss_selectors[!ss_selectors %in% remove_ss]
          # Only save those selectors which are used by plot
          for(ss in ss_selectors){
            if(!ss %in% AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors){
              AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors <-
                c(ss, AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors)
            }
          }
          ## Set up built_data to compute domains
          built_data <- a_plot.list[[p.name]]$built$plot$layers[[num]]$data
          built_data$PANEL <- a_plot.list[[p.name]]$built$data[[num]]$PANEL
          ## since geom names are now like geom_a_point strsplit changes to [[1]][[3]]
          if(length(ss_selectors) > 0){
            subset_domains[num] <- compute_domains(
              built_data,
              axis, strsplit(names(g.list[[p.name]])[[num]], "_")[[1]][[3]],
              names(sort(ss_selectors)), split_by_panel, g.list[[p.name]][[num]]$g$a_aes)
          }
        }
        subset_domains <- subset_domains[!sapply(subset_domains, is.null)]
        if(length(subset_domains) > 0){
          use_domain <- get_domain(subset_domains)
          # Save for renderer
          AllPlotsInfo[[p.name]]$axis_domains[[axis]]$domains <- use_domain
          # Get gridlines for updates
          AllPlotsInfo[[p.name]]$axis_domains[[axis]]$grids <- 
            get_ticks_gridlines(use_domain)
          ## Initially selected selector values are stored in curr_select
          ## which updates every time a user updates the axes
          saved_selectors <- sort(names(meta$selectors))
          for (ss in saved_selectors){
            if(ss %in% AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors){
              AllPlotsInfo[[p.name]]$axis_domains[[axis]]$curr_select[[ss]] <-
                meta$selectors[[ss]]$selected
            }
          }
        }else{
          warning(paste("update_axes specified for", toupper(axis),
            "axis on plot", p.name, 
            "but found no geoms with showSelected=singleSelectionVariable,",
            "so created a plot with no updates for",
            toupper(axis), "axis"), call. = FALSE)
          # Do not save in plot.json file if axes is not getting updated
          update_axes <- AllPlotsInfo[[p.name]]$options$update_axes
          AllPlotsInfo[[p.name]]$options$update_axes <-
            update_axes[!axis == update_axes]
        }
      }
    }
  }
  
  ## Finally save all the layers 
  for(p.name in names(a_plot.list)){
    for(g1 in seq_along(g.list[[p.name]])){
      g <- storeLayer(meta, g.list[[p.name]][[g1]]$g,
                      g.list[[p.name]][[g1]]$g.data.varied)
      ## Every plot has a list of geom names.
      AllPlotsInfo[[p.name]]$geoms <- c(
        AllPlotsInfo[[p.name]]$geoms, list(g$classed))
    }#layer.i
  }
  
  ## Now that selectors are all defined, go back through geoms to
  ## check if there are any warnings to issue.
  issueSelectorWarnings(meta$geoms, meta$selector.a_aes, meta$duration)
  
  ## These geoms need to be updated when the time.var is animated, so
  ## let's make a list of all possible values to cycle through, from
  ## all the values used in those geoms.
  if("time" %in% names(AnimationInfo)){
    meta$selectors[[AnimationInfo$time$variable]]$type <- "single"
    anim.values <- AnimationInfo$timeValues
    if(length(AnimationInfo$timeValues)==0){
      stop("no interactive aes for time variable ", AnimationInfo$time$variable)
    }
    anim.not.null <- anim.values[!sapply(anim.values, is.null)]
    time.classes <- sapply(anim.not.null, function(x) class(x)[1])
    time.class <- time.classes[[1]]
    if(any(time.class != time.classes)){
      print(time.classes)
      stop("time variables must all have the same class")
    }
    AnimationInfo$time$sequence <- if(time.class=="POSIXct"){
      orderTime <- function(format){
        values <- unlist(sapply(anim.not.null, strftime, format))
        sort(unique(as.character(values)))
      }
      hms <- orderTime("%H:%M:%S")
      f <- if(length(hms) == 1){
        "%Y-%m-%d"
      }else{
        "%Y-%m-%d %H:%M:%S"
      }
      orderTime(f)
    }else if(time.class=="factor"){
      levs <- levels(anim.not.null[[1]])
      if(any(sapply(anim.not.null, function(f)levels(f)!=levs))){
        print(sapply(anim.not.null, levels))
        stop("all time factors must have same levels")
      }
      levs
    }else{ #character, numeric, integer, ... what else?
      as.character(sort(unique(unlist(anim.not.null))))
    }
    meta$selectors[[time.var]]$selected <- AnimationInfo$time$sequence[[1]]
  }
  
  ## The first selection:
  for(selector.name in names(meta$first)){
    first <- as.character(meta$first[[selector.name]])
    if(selector.name %in% names(meta$selectors)){
      s.type <- meta$selectors[[selector.name]]$type
      if(s.type == "single"){
        stopifnot(length(first) == 1)
      }
      meta$selectors[[selector.name]]$selected <- first
    }else{
      print(list(selectors=names(meta$selectors),
                 missing.first=selector.name))
      stop("missing first selector variable")
    }
  }
  
  meta$plots <- AllPlotsInfo
  meta$time <- AnimationInfo$time
  meta$timeValues <- AnimationInfo$timeValues
  ## Finally, copy html/js/json files to out.dir.
  src.dir <- system.file("htmljs",package="animint2")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  if(file.exists(paste0(out.dir, "styles.css")) | css.file != "default.file"){
    to.copy <- to.copy[!grepl("styles.css", to.copy, fixed=TRUE)]
  }
  if(css.file!=""){
    # if css filename is provided, copy that file to the out directory as "styles.css"
    to.copy <- to.copy[!grepl("styles.css", to.copy, fixed=TRUE)]
    if(!file.exists(css.file)){
      stop(paste("css.file", css.file, "does not exist. Please check that the file name and path are specified correctly."))
    } else {
      file.copy(css.file, file.path(out.dir, "styles.css"), overwrite=TRUE)
    }
  } else {
    style.file <- system.file("htmljs", "styles.css", package = "animint2")
    file.copy(style.file, file.path(out.dir, "styles.css"), overwrite=TRUE)
  }
  file.copy(to.copy, out.dir, overwrite=TRUE, recursive=TRUE)
  export.names <-
    c("geoms", "time", "duration", "selectors", "plots", "title")
  export.data <- list()
  for(export.name in export.names){
    if(export.name %in% ls(meta)){
      export.data[[export.name]] <- meta[[export.name]]
    }
  }
  json <- RJSONIO::toJSON(export.data)
  cat(json, file = file.path(out.dir, json.file))
  if (open.browser) {
    message('opening a web browser with a file:// URL; ',
            'if the web page is blank, try running
if (!requireNamespace("servr")) install.packages("servr")
servr::httd("', normalizePath( out.dir,winslash="/" ), '")')
      browseURL(sprintf("%s/index.html", out.dir))
  }
  
  ## After everything has been done, we restore the original mappings
  ## in the plot.list. This is necessary for visualizations where we use
  ## the same plot.list with minor edits in another viz
  ## See this comment:
  ## https://github.com/tdhock/animint2/pull/5#issuecomment-323074518
  for(plot_i in a_plot.list){
    for(a_layer_i in seq_along(plot_i$a_plot$layers)){
      plot_i$a_plot$layers[[a_layer_i]]$mapping <-
        plot_i$a_plot$layers[[a_layer_i]]$orig_mapping
    } 
  }
  invisible(meta)
  ### An invisible copy of the R list that was exported to JSON.
}


#' Function to get legend information from a_plot
#' @param plistextra output from a_plot_build(p)
#' @return list containing information for each legend
#' @export
getLegendList <- function(plistextra){
  plot <- plistextra$plot
  scales <- plot$scales
  layers <- plot$layers
  default_mapping <- plot$mapping
  a_theme <- plot_a_theme(plot)
  a_position <- a_theme$legend.a_position
  # by default, guide boxes are vertically aligned
  if(is.null(a_theme$legend.box)) a_theme$legend.box <- "vertical" else a_theme$legend.box

  # size of key (also used for bar in colorbar guide)
  if(is.null(a_theme$legend.key.width)) a_theme$legend.key.width <- a_theme$legend.key.size
  if(is.null(a_theme$legend.key.height)) a_theme$legend.key.height <- a_theme$legend.key.size
  # by default, direction of each guide depends on the position of the guide.
  if(is.null(a_theme$legend.direction)){
    a_theme$legend.direction <- 
      if (length(a_position) == 1 && a_position %in% c("top", "bottom", "left", "right"))
        switch(a_position[1], top =, bottom = "horizontal", left =, right = "vertical")
    else
      "vertical"
  }
  # justification of legend boxes
  a_theme$legend.box.just <-
    if(is.null(a_theme$legend.box.just)) {
      if (length(a_position) == 1 && a_position %in% c("top", "bottom", "left", "right"))
        switch(a_position, bottom =, top = c("center", "top"), left =, right = c("left", "top"))
      else
        c("center", "center")
    }

  a_position <- a_theme$legend.a_position
  # locate guide argument in a_scale_*, and use that for a default.
  # Note, however, that guides(colour = ...) has precendence! See https://gist.github.com/cpsievert/ece28830a6c992b29ab6
  a_guides.args <- list()
  for(a_aes.name in c("colour", "fill")){
    a_aes.loc <- which(scales$find(a_aes.name))
    a_guide.type <- if (length(a_aes.loc) == 1){
      scales$scales[[a_aes.loc]][["a_guide"]]
    }else{
      "legend"
    }
    if(a_guide.type=="colourbar")a_guide.type <- "legend"
    a_guides.args[[a_aes.name]] <- a_guide.type
  }
  a_guides.result <- do.call(a_guides, a_guides.args)
  a_guides.list <- plyr::defaults(plot$a_guides, a_guides.result)
  gdefs <- a_guides_train(scales = scales,
                           a_theme = a_theme,
                           a_guides = a_guides.list,
                           a_labels = plot$a_labels)
  if (length(gdefs) != 0) {
    gdefs <- a_guides_merge(gdefs)
    gdefs <- a_guides_geom(gdefs, layers, default_mapping)
  } else (a_zeroGrob())
  names(gdefs) <- sapply(gdefs, function(i) i$title)
  
  ## adding the variable used to each LegendList
  for(leg in seq_along(gdefs)) {
    legend_type <- names(gdefs[[leg]]$key)
    legend_type <- legend_type[legend_type != ".a_label"]
    gdefs[[leg]]$legend_type <- legend_type
    a_scale.list <- scales$scales[which(scales$find(legend_type))]
    discrete.vec <- sapply(a_scale.list, inherits, "a_ScaleDiscrete")
    is.discrete <- all(discrete.vec)
    gdefs[[leg]]$is.discrete <- is.discrete
    ## get the name of the legend/selection variable.
    var.list <- list()
    for(a_layer.i in seq_along(plot$layers)) {
      L <- plot$layers[[a_layer.i]]
      var.list[[a_layer.i]] <- L$mapping[legend_type]
    }
    unique.var.list <- unique(unlist(var.list))
    if(is.discrete){
      var.name <- unique.var.list[[1]]
      if(length(unique.var.list) == 1 && is.symbol(var.name)){
        gdefs[[leg]]$selector <- paste(var.name)
      }else{
        str(unique.var.list)
        stop("need exactly 1 variable name ",
             "(not constant, not language/expression) ",
             "to create interactive discrete legend for aes ",
             paste(legend_type, collapse=", "))
      }
    }
    
    ## do not draw geoms which are constant:
    a_geom.list <- gdefs[[leg]]$geoms
    a_geom.data.list <- lapply(a_geom.list, "[[", "data")
    a_geom.data.rows <- sapply(a_geom.data.list, nrow)
    a_geom.unique.list <- lapply(a_geom.data.list, unique)
    a_geom.unique.rows <- sapply(a_geom.unique.list, nrow)
    is.ignored <- 1 < a_geom.data.rows & a_geom.unique.rows == 1
    gdefs[[leg]]$geoms <- a_geom.list[!is.ignored]
    
    ## Pass a a_geom.legend.list to be used by the
    ## GetLegend function
    a_geom.legend.list <- list()
    for(a_geom.i in seq_along(gdefs[[leg]]$geoms)){
      data.a_geom.i <- gdefs[[leg]]$geoms[[a_geom.i]]$data
      params.a_geom.i <- gdefs[[leg]]$geoms[[a_geom.i]]$params
      size.a_geom.i <- gdefs[[leg]]$geoms[[a_geom.i]]$size
      
      suppressWarnings(draw.key.used <- 
                         gdefs[[leg]]$geoms[[a_geom.i]]$draw_key(
                           data.a_geom.i, params.a_geom.i, size.a_geom.i)
      )
      a_geom.legend <- class(draw.key.used)[[1]]
      a_geom.legend.list <- c(a_geom.legend.list, a_geom.legend)
    }
    
    ## Process names to be used by the CleanData function
    convert.names.list <- list(points="point", segments="path", rect="polygon")
    names.to.change <- a_geom.legend.list %in% names(convert.names.list)
    a_geom.legend.list[names.to.change] <- 
      convert.names.list[unlist(a_geom.legend.list[names.to.change])]
    
    gdefs[[leg]]$a_geom.legend.list <- a_geom.legend.list
  }
  
  ## Add a flag to specify whether or not breaks was manually
  ## specified. If it was, then it should be respected. If not, and
  ## the legend shows a numeric variable, then it should be reversed.
  for(legend.name in names(gdefs)){
    key.df <- gdefs[[legend.name]]$key
    a_aes.name <- names(key.df)[1]
    a_scale.i <- which(scales$find(a_aes.name))
    if(length(a_scale.i) == 1){
      sc <- scales$scales[[a_scale.i]]
      gdefs[[legend.name]]$breaks <- sc$breaks
    }
  }

  legend.list <- lapply(gdefs, getLegend)
  ## Add a flag to specify whether or not there is both a color and a
  ## fill legend to display. If so, we need to draw the interior of
  ## the points in the color legend as the same color.
  if(1 < length(legend.list)){
    is.color <- sapply(legend.list, function(L)"colour" %in% L$legend_type)
    is.fill <- sapply(legend.list, function(L)"fill" %in% L$legend_type)
    is.point <- sapply(legend.list, function(L)"point" %in% L$geoms)
    has.both <- 2 == sum(is.point & (is.color | is.fill))
    if(has.both){
      for(legend.i in which(is.color)){
        entry.list <- legend.list[[legend.i]]$entries
        for(entry.i in seq_along(entry.list)){
          entry <- entry.list[[entry.i]]
          color.names <- grep("colour", names(entry), value=TRUE)
          fill.names <- sub("colour", "fill", color.names)
          entry[fill.names] <- "#FFFFFF"
          legend.list[[legend.i]]$entries[[entry.i]] <- entry
        }
      }
    }
  }
  legend.list[0 < sapply(legend.list, length)]
}
