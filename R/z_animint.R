#' Convert a ggplot to a list.
#' @param meta environment with previously calculated plot data, and a new plot to parse, already stored in plot and plot.name.
#' @param plot ggplot list object
#' @param plot.name name of plot
#' @return nothing, info is stored in meta.
#' @export
#' @import plyr
parsePlot <- function(meta, plot, plot.name){
  ## adding data and mapping to each layer from base plot, if necessary
  for(layer.i in seq_along(plot$layers)) {

    ## if data is not specified, get it from plot
    if(length(plot$layers[[layer.i]]$data) == 0){
      plot$layers[[layer.i]]$data <- plot$data
    }

    ## if mapping is not specified, get it from plot
    if(is.null(plot$layers[[layer.i]]$mapping)){
      plot$layers[[layer.i]]$mapping <- plot$mapping
    }
  }

  built <- ggplot_build(plot)
  plot.info <- list()

  ## Export axis specification as a combination of breaks and
  ## labels, on the relevant axis scale (i.e. so that it can
  ## be passed into d3 on the x axis scale instead of on the
  ## grid 0-1 scale). This allows transformations to be used
  ## out of the box, with no additional d3 coding.
  theme.pars <- plot_theme(plot)

  ## Interpret panel.margin as the number of lines between facets
  ## (ignoring whatever grid::unit such as cm that was specified).

  ## Now ggplot specifies panel.margin in 'pt' instead of 'lines'
  plot.info$panel_margin_lines <- pt.to.lines(theme.pars$panel.margin)

  ## No legend if theme(legend.postion="none").
  plot.info$legend <- if(theme.pars$legend.position != "none"){
    getLegendList(built)
  }

  ## scan for legends in each layer.
  for(layer.i in seq_along(plot$layers)){
    ##cat(sprintf("%4d / %4d layers\n", layer.i, length(plot$layers)))
    ## This is the layer from the original ggplot object.
    L <- plot$layers[[layer.i]]

    ## Use original mapping saved before calling parsePlot
    ## This is to handle cases where the plots may share the same layer
    ## If the layer mapping in one plot has been edited by the animint
    ## compiler, the layer mapping in the other plots will also change
    ## which will give error when we check if showSelected/clickSelects have
    ## been used as aesthetics
    L$mapping <- L$orig_mapping
    class(L$mapping) <- "list"

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
    ## mapping before calling ggplot_build
    L$mapping <- addSSandCSasAesthetics(L$mapping, L$extra_params)
  }#layer.i

  ## need to call ggplot_build again because we've added to the plot.
  ## I'm sure that there is a way around this, but not immediately sure how.
  ## There's sort of a Catch-22 here because to create the interactivity,
  ## we need to specify the variable corresponding to each legend.
  ## To do this, we need to have the legend.
  ## And to have the legend, I think that we need to use ggplot_build
  built <- ggplot_build(plot)
  ## TODO: implement a compiler that does not call ggplot_build at
  ## all, and instead does all of the relevant computations in animint
  ## code.
  ## 'strips' are really titles for the different facet panels
  plot.info$strips <- with(built, getStrips(plot$facet, panel))

  ## the layout tells us how to subset and where to plot on the JS side
  plot.info$layout <- with(built, flag_axis(plot$facet, panel$layout))
  plot.info$layout <- with(built, train_layout(
    plot$facet, plot$coordinates, plot.info$layout, panel$ranges))

  # saving background info
  plot.info$panel_background <- get_bg(theme.pars$panel.background, theme.pars)
  plot.info$panel_border <- get_bg(theme.pars$panel.border, theme.pars)

  # extract major grid lines
  plot.info$grid_major <- get_grid(theme.pars$panel.grid.major, theme.pars,
                                   plot.info, meta, built)
  # extract minor grid lines
  plot.info$grid_minor <- get_grid(theme.pars$panel.grid.minor, theme.pars,
                                   plot.info, meta, built, major = F)

  ## Flip labels if coords are flipped - transform does not take care
  ## of this. Do this BEFORE checking if it is blank or not, so that
  ## individual axes can be hidden appropriately, e.g. #1.
  if("CoordFlip"%in%attr(plot$coordinates, "class")){
    temp <- plot$labels$x
    plot$labels$x <- plot$labels$y
    plot$labels$y <- temp
  }
  is.blank <- function(el.name){
    x <- calc_element(el.name, plot$theme)
    "element_blank"%in%attr(x,"class")
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
      scale.i <- which(plot$scales$find(xy))
      lab.or.null <- if(length(scale.i) == 1){
        plot$scales$scales[[scale.i]]$name
      }
      if(is.null(unlist(lab.or.null))){
        plot$labels[[xy]]
      }else{
        lab.or.null
      }
    }
    # theme settings are shared across panels
    axis.text <- theme.pars[[s("axis.text.%s")]]
    ## TODO: also look at axis.text! (and text?)
    size <- if(is.numeric(axis.text$size)){
      axis.text$size # defined in pts
    } else if(is.rel(axis.text$size)){
      axis.text$size * 11 # reletive size = scale number * default size
    }
    anchor <- hjust2anchor(axis.text$hjust)
    angle <- if(is.numeric(axis.text$angle)){
      -axis.text$angle
    }
    if(is.null(size)){
      size <- 11
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
    plot.info[[s("%ssize")]] <- as.numeric(size)
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
        as.list(range[[s("%s.labels")]])
      }
      plot.info[[axis]][[s("%srange")]] <- range[[s("%s.range")]]
      plot.info[[axis]][[s("%sline")]] <- !is.blank(s("axis.line.%s"))
      plot.info[[axis]][[s("%sticks")]] <- !is.blank(s("axis.ticks.%s"))
    }
  }
  # grab the unique axis labels (makes rendering simpler)
  plot.info <- getUniqueAxisLabels(plot.info)

  # grab plot title if present
  plot.info$title <- getPlotTitle(theme.pars$plot.tiltle,
                                  plot$labels$title)

  ## Set plot width and height from animint.* options if they are
  ## present.
  options_list <- getWidthAndHeight(plot$theme)
  options_list <- setUpdateAxes(plot$theme, options_list)
  plot.info$options <- options_list

  list(
    plot.info=plot.info,
    ggplot=plot,
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

#' Compile and render an animint in a local directory.
#'
#' This function converts an animint plot.list into a directory of
#' files which can be used to render the interactive data
#' visualization in a web browser.
#' @param plot.list a named list of ggplots and option lists.
#' @param out.dir directory to store html/js/csv files. If it exists
#'   already, it will be removed before writing the new
#'   directory/files.
#' @param json.file character string that names the JSON file with
#'   metadata associated with the plot.
#' @param open.browser Should R open a browser? If yes, be sure to
#'   configure your browser to allow access to local files, as some
#'   browsers block this by default (e.g. chrome).
#' @param css.file character string for non-empty css file to
#'   include. Provided file will be copied to the output directory as
#'   styles.css
#' @return invisible list of ggplots in list format.
#' @export
#' @import RJSONIO
#' @importFrom utils browseURL head packageVersion str tail
#'   write.table
#' @example inst/examples/animint2dir.R
animint2dir <- function(plot.list, out.dir = NULL,
                        json.file = "plot.json", open.browser = interactive(),
                        css.file = "") {
  if(is.null(out.dir)){
    out.dir <- tempfile()
  }
  unlink(out.dir, recursive=TRUE)
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
  if(!is.null(plot.list$out.dir)){
    plot.list$out.dir <- NULL
  }

  ## Extract essential info from ggplots, reality checks.
  for(list.name in names(plot.list)){
    p <- plot.list[[list.name]]
    if(is.ggplot(p)){
      ## Save original mapping to every layer so that we can use it afterwards
      ## This is required because we edit the original plot list created for the
      ## animint2dir function. See the discussion here:
      ## https://github.com/tdhock/animint2/pull/5#issuecomment-323072502
      for(layer_i in seq_along(p$layers)){
        ## Viz has not been used before
        if(is.null(p$layers[[layer_i]]$orig_mapping)){
          p$layers[[layer_i]]$orig_mapping <-
            if(is.null(p$layers[[layer_i]]$mapping)){
              ## Get mapping from plot if not defined in layer
              p$mapping
            }else{
              p$layers[[layer_i]]$mapping
            }
        }else{
          ## This is not the first time this layer is being processed, so we replace
          ## the mapping with the original mapping here
          p$layers[[layer_i]]$mapping <- p$layers[[layer_i]]$orig_mapping
        }
      }

      ## Before calling ggplot_build, we do some error checking for
      ## some animint extensions.
      checkPlotForAnimintExtensions(p, list.name)
    }else if(is.list(p)){ ## for options.
      meta[[list.name]] <- p
    }else{
      stop("list items must be ggplots or option lists, problem: ", list.name)
    }
  }

  ## Call ggplot_build in parsPlot for all ggplots
  ggplot.list <- list()
  AllPlotsInfo <- list()
  for(list.name in names(plot.list)){
    p <- plot.list[[list.name]]
    if(is.ggplot(p)){
      ## If plot is correct, save to meta for further processing
      parsed_info <- parsePlot(meta, p, list.name) # calls ggplot_build.
      AllPlotsInfo[[list.name]] <- parsed_info$plot.info
      ggplot.list[[list.name]]$ggplot <- parsed_info$ggplot
      ggplot.list[[list.name]]$built <- parsed_info$built
    }
  }

  ## After going through all of the meta-data in all of the ggplots,
  ## now we have enough info to save the TSV file database.
  geom_num <- 0
  g.list <- list()
  for(p.name in names(ggplot.list)){
    ggplot.info <- ggplot.list[[p.name]]
    for(layer.i in seq_along(ggplot.info$ggplot$layers)){
      L <- ggplot.info$ggplot$layers[[layer.i]]
      df <- ggplot.info$built$data[[layer.i]]
      ## Data now contains columns with fill, alpha, colour etc.
      ## Remove from data if they have a single unique value and
      ## are NOT used in mapping to reduce tsv file size
      redundant.cols <- names(L$geom$default_aes)
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
      layer_name <- getLayerName(L, geom_num, p.name)
      if(nrow(df)==0){
        stop("no data in ", layer_name)
      }
      gl <- Geom$export_animint(
        L, df, meta, layer_name,
        ggplot.info$ggplot, ggplot.info$built, AnimationInfo)
      ## Save Animation Info separately
      AnimationInfo$timeValues <- gl$timeValues
      gl$timeValues <- NULL
      ## Save to a list before saving to tsv
      ## Helps during axis updates and Inf values
      g.list[[p.name]][[gl$g$classed]] <- gl
    }#layer.i
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
      ## (if .variable .value aes, then no selectize; otherwise if
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

  ## For a static data viz with no interactive aes, no need to check
  ## for trivial showSelected variables with only 1 level.
  checkSingleShowSelectedValue(meta$selectors)

  ## Go through options and add to the list.
  for(v.name in names(meta$duration)){
    meta$selectors[[v.name]]$duration <- meta$duration[[v.name]]
  }
  ## Set plot sizes.
  setPlotSizes(meta, AllPlotsInfo)

  ## Get domains of data subsets if theme_animint(update_axes) is used
  for(p.name in names(ggplot.list)){
    axes_to_update <- AllPlotsInfo[[p.name]]$options$update_axes
    if(!is.null(axes_to_update)){
      for (axis in axes_to_update){
        subset_domains <- list()
        # Determine if every panel needs a different domain or not
        # We conclude here if we want to split the data by PANEL
        # for the axes updates. Else every panel uses the same domain
        panels <- ggplot.list[[p.name]]$built$panel$layout$PANEL
        axes_drawn <-
          ggplot.list[[p.name]]$built$panel$layout[[
            paste0("AXIS_", toupper(axis))]]
        panels_used <- panels[axes_drawn]
        split_by_panel <- all(panels == panels_used)
        for(layer.i in seq_along(ggplot.list[[p.name]]$built$plot$layers)){
          ## If there is a geom where the axes updates have non numeric values,
          ## we stop and throw an informative warning
          ## It does not make sense to have axes updates for non numeric values
          aesthetic_names <- names(g.list[[p.name]][[layer.i]]$g$aes)
          axis_pattern <- paste0("^", axis)
          axis_col_name <- grep(axis_pattern, aesthetic_names, value=TRUE)
          if(0==length(axis_col_name)){
            ##geom_abline does not have any x/y aes to contribute to
            ##the scale computations so we just ignore this layer --
            ##TODO all of this logic for computing the axes updates
            ##should be moved to the geom classes.
          }else{
            first_axis_name <- axis_col_name[[1]]
            axis_col <- g.list[[p.name]][[layer.i]]$g$aes[[first_axis_name]]
            axis_is_numeric <- is.numeric(
              ggplot.list[[p.name]]$built$plot$layers[[
                layer.i]]$data[[axis_col]])
            if(!axis_is_numeric){
              stop(paste0(
                "'update_axes' specified for '", toupper(axis),
                "' axis on plot '", p.name,
                "' but the column '", axis_col, "' is non-numeric.",
                " Axes updates are only available for numeric data."))
            }
            ## handle cases for showSelected: showSelectedlegendfill,
            ## showSelectedlegendcolour etc.
            choose_ss <- grepl("^showSelected", aesthetic_names)
            ss_selectors <- g.list[[p.name]][[layer.i]]$g$aes[choose_ss]
            ## Do not calculate domains for multiple selectors
            remove_ss <- c()
            for(j in seq_along(ss_selectors)){
              if(meta$selectors[[ ss_selectors[[j]] ]]$type != "single"){
                remove_ss <- c(remove_ss, ss_selectors[j])
              }
            }
            ss_selectors <- ss_selectors[!ss_selectors %in% remove_ss]
            ## Only save those selectors which are used by plot
            for(ss in ss_selectors){
              if(!ss %in% AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors){
                AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors <- c(
                  ss, AllPlotsInfo[[p.name]]$axis_domains[[axis]]$selectors)
              }
            }
            ## Set up built_data to compute domains
            built_data <-
              ggplot.list[[p.name]]$built$plot$layers[[layer.i]]$data
            built_data$PANEL <-
              ggplot.list[[p.name]]$built$data[[layer.i]]$PANEL
            if(length(ss_selectors) > 0){
              subset_domains[layer.i] <- compute_domains(
                built_data,
                axis,
                strsplit(names(g.list[[p.name]])[[layer.i]], "_")[[1]][[2]],
                names(sort(ss_selectors)),
                split_by_panel,
                g.list[[p.name]][[layer.i]]$g$aes)
            }
          }
        }##for(layer.i
        subset_domains <- subset_domains[!sapply(subset_domains, is.null)]
        if(length(subset_domains)==0){
          warning(paste("update_axes specified for", toupper(axis),
            "axis on plot", p.name,
            "but found no geoms with showSelected=singleSelectionVariable,",
            "so created a plot with no updates for",
            toupper(axis), "axis"), call. = FALSE)
          # Do not save in plot.json file if axes is not getting updated
          update_axes <- AllPlotsInfo[[p.name]]$options$update_axes
          AllPlotsInfo[[p.name]]$options$update_axes <-
            update_axes[!axis == update_axes]
        }else{
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
        }
      }
    }
  }

  ## Finally save all the layers
  for(p.name in names(ggplot.list)){
    for(g1 in seq_along(g.list[[p.name]])){
      g <- storeLayer(meta, g.list[[p.name]][[g1]]$g,
                      g.list[[p.name]][[g1]]$g.data.varied)
      ## Every plot has a list of geom names.
      AllPlotsInfo[[p.name]]$geoms <- c(
        AllPlotsInfo[[p.name]]$geoms, list(g$classed))
    }
  }

  ## Now that selectors are all defined, go back through geoms to
  ## check if there are any warnings to issue.
  issueSelectorWarnings(meta$geoms, meta$selector.aes, meta$duration)

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
    u <- normalizePath(file.path(out.dir, "index.html"))
    browseURL(u)
  }

  ## After everything has been done, we restore the original mappings
  ## in the plot.list. This is necessary for visualizations where we use
  ## the same plot.list with minor edits in another viz
  ## See this comment:
  ## https://github.com/tdhock/animint2/pull/5#issuecomment-323074518
  for(plot_i in ggplot.list){
    for(layer_i in seq_along(plot_i$ggplot$layers)){
      plot_i$ggplot$layers[[layer_i]]$mapping <-
        plot_i$ggplot$layers[[layer_i]]$orig_mapping
    }
  }
  invisible(meta)
  ### An invisible copy of the R list that was exported to JSON.
}


#' Function to get legend information from ggplot
#' @param plistextra output from ggplot_build(p)
#' @return list containing information for each legend
#' @export
getLegendList <- function(plistextra){
  plot <- plistextra$plot
  scales <- plot$scales
  layers <- plot$layers
  default_mapping <- plot$mapping
  theme <- plot_theme(plot)
  position <- theme$legend.position
  # by default, guide boxes are vertically aligned
  if(is.null(theme$legend.box)) theme$legend.box <- "vertical" else theme$legend.box

  # size of key (also used for bar in colorbar guide)
  if(is.null(theme$legend.key.width)) theme$legend.key.width <- theme$legend.key.size
  if(is.null(theme$legend.key.height)) theme$legend.key.height <- theme$legend.key.size
  # by default, direction of each guide depends on the position of the guide.
  if(is.null(theme$legend.direction)){
    theme$legend.direction <-
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position[1], top =, bottom = "horizontal", left =, right = "vertical")
    else
      "vertical"
  }
  # justification of legend boxes
  theme$legend.box.just <-
    if(is.null(theme$legend.box.just)) {
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position, bottom =, top = c("center", "top"), left =, right = c("left", "top"))
      else
        c("center", "center")
    }

  position <- theme$legend.position
  # locate guide argument in scale_*, and use that for a default.
  # Note, however, that guides(colour = ...) has precendence! See https://gist.github.com/cpsievert/ece28830a6c992b29ab6
  guides.args <- list()
  for(aes.name in c("colour", "fill")){
    aes.loc <- which(scales$find(aes.name))
    guide.type <- if (length(aes.loc) == 1){
      scales$scales[[aes.loc]][["guide"]]
    }else{
      "legend"
    }
    if(guide.type=="colourbar")guide.type <- "legend"
    guides.args[[aes.name]] <- guide.type
  }
  guides.result <- do.call(guides, guides.args)
  guides.list <- plyr::defaults(plot$guides, guides.result)
  gdefs <- guides_train(scales = scales,
                           theme = theme,
                           guides = guides.list,
                           labels = plot$labels)
  if (length(gdefs) != 0) {
    gdefs <- guides_merge(gdefs)
    gdefs <- guides_geom(gdefs, layers, default_mapping)
  } else (zeroGrob())
  names(gdefs) <- sapply(gdefs, function(i) i$title)

  ## adding the variable used to each LegendList
  for(leg in seq_along(gdefs)) {
    legend_type <- names(gdefs[[leg]]$key)
    legend_type <- legend_type[legend_type != ".label"]
    gdefs[[leg]]$legend_type <- legend_type
    scale.list <- scales$scales[which(scales$find(legend_type))]
    discrete.vec <- sapply(scale.list, inherits, "ScaleDiscrete")
    is.discrete <- all(discrete.vec)
    gdefs[[leg]]$is.discrete <- is.discrete
    ## get the name of the legend/selection variable.
    var.list <- list()
    for(layer.i in seq_along(plot$layers)) {
      L <- plot$layers[[layer.i]]
      var.list[[layer.i]] <- L$mapping[legend_type]
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
    geom.list <- gdefs[[leg]]$geoms
    geom.data.list <- lapply(geom.list, "[[", "data")
    geom.data.rows <- sapply(geom.data.list, nrow)
    geom.unique.list <- lapply(geom.data.list, unique)
    geom.unique.rows <- sapply(geom.unique.list, nrow)
    is.ignored <- 1 < geom.data.rows & geom.unique.rows == 1
    gdefs[[leg]]$geoms <- geom.list[!is.ignored]

    ## Pass a geom.legend.list to be used by the
    ## GetLegend function
    geom.legend.list <- list()
    for(geom.i in seq_along(gdefs[[leg]]$geoms)){
      data.geom.i <- gdefs[[leg]]$geoms[[geom.i]]$data
      params.geom.i <- gdefs[[leg]]$geoms[[geom.i]]$params
      size.geom.i <- gdefs[[leg]]$geoms[[geom.i]]$size

      suppressWarnings(draw.key.used <-
                         gdefs[[leg]]$geoms[[geom.i]]$draw_key(
                           data.geom.i, params.geom.i, size.geom.i)
      )
      geom.legend <- class(draw.key.used)[[1]]
      geom.legend.list <- c(geom.legend.list, geom.legend)
    }

    ## Process names to be used by the CleanData function
    convert.names.list <- list(points="point", segments="path", rect="polygon")
    names.to.change <- geom.legend.list %in% names(convert.names.list)
    geom.legend.list[names.to.change] <-
      convert.names.list[unlist(geom.legend.list[names.to.change])]

    gdefs[[leg]]$geom.legend.list <- geom.legend.list
  }

  ## Add a flag to specify whether or not breaks was manually
  ## specified. If it was, then it should be respected. If not, and
  ## the legend shows a numeric variable, then it should be reversed.
  for(legend.name in names(gdefs)){
    key.df <- gdefs[[legend.name]]$key
    aes.name <- names(key.df)[1]
    scale.i <- which(scales$find(aes.name))
    if(length(scale.i) == 1){
      sc <- scales$scales[[scale.i]]
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
