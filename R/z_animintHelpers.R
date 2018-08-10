## Animint specific helper functions

#' Add a showSelected aesthetic if legend is specified
#' @param meta meta object with all information
#' @param legend legend to scan for showSelected
#' @param L layer of the plot
#' @return L : Layer with additional mapping to new aesthetic
addShowSelectedForLegend <- function(meta, legend, L){
  for(legend.i in seq_along(legend)) {
    one.legend <- legend[[legend.i]]
    ## the name of the selection variable used in this legend.
    s.name <- one.legend$selector
    is.variable.name <- is.character(s.name) && length(s.name) == 1
    a_layer.has.variable <- s.name %in% names(L$data)
    
    if(is.variable.name && a_layer.has.variable) {
      ## grabbing the variable from the data
      var <- L$data[, s.name]
      is.interactive.a_aes <-
        grepl("showSelected|clickSelects", names(L$mapping))
      is.legend.var <- L$mapping == s.name
      ## If s.name is used with another interactive a_aes, then do
      ## not add any showSelected aesthetic for it.
      var.is.interactive <- any(is.interactive.a_aes & is.legend.var)
      if(!var.is.interactive){
        ## only add showSelected aesthetic if the variable is
        ## used by the a_geom
        type.vec <- one.legend$legend_type
        if(any(type.vec %in% names(L$mapping))){
          type.str <- paste(type.vec, collapse="")
          a.name <- paste0("showSelectedlegend", type.str)
          L$mapping[[a.name]] <- as.symbol(s.name)
        }
      }
      ## if selector.types has not been specified, create it
      if(is.null(meta$selector.types)) {
        meta$selector.types <- list()
      }
      ## if selector.types is not specified for this variable, set
      ## it to multiple.
      if(is.null(meta$selector.types[[s.name]])) {
        meta$selector.types[[s.name]] <- "multiple"
        meta$selectors[[s.name]]$type <- "multiple"
      }
      ## if first is not specified, create it
      if(is.null(meta$first)) {
        meta$first <- list()
      }
      ## if first is not specified, add all to first
      if(is.null(meta$first[[s.name]])) {
        u.vals <- unique(var)
      }
      ## Tell this selector that it has a legend somewhere in the
      ## viz. (if the selector has no interactive legend and no
      ## clickSelects, then we show the widgets by default).
      meta$selectors[[s.name]]$legend <- TRUE
    }#length(s.name)
  }#legend.i
  return(L)
}


## extract panel background and borders from a_theme.pars
get_bg <- function(pars, a_theme.pars) {
  # if pars is not an empty list - occurs when using a_element_blank()
  if(length(pars) > 0) {
    
    ## if elements are not specified, they inherit from a_theme.pars$rect
    for(i in 1:length(pars)) {
      if(is.null(pars[[i]])) pars[[i]] <- unname(a_theme.pars$rect[[i]])
    }
    
    # convert fill to RGB if necessary
    if(!(is.rgb(pars$fill))) pars$fill <- unname(toRGB(pars$fill))
    # convert color to RGB if necessary
    if(!(is.rgb(pars$colour))) pars$colour <- unname(toRGB(pars$colour))
    
    # remove names (JSON file was getting confused)
    pars <- lapply(pars, unname)
    
  }
  pars
}


### function to extract grid info
get_grid <- function(pars, a_theme.pars, plot.meta, meta, built, major = T) {
  # if pars is not an empty list - occurs when using a_element_blank()
  if(length(pars) > 0) {
    
    ## if elements are not specified, they inherit from 
    ##    a_theme.pars$panel.grid then from a_theme.pars$line
    for(i in names(pars)) {
      if(is.null(pars[[i]])) pars[[i]] <- 
          if(!is.null(a_theme.pars$panel.grid[[i]])) {
            a_theme.pars$panel.grid[[i]]
          } else {
            a_theme.pars$line[[i]]
          }
    }
    # convert colour to RGB if necessary
    if(!is.rgb(pars$colour)) pars$colour <- unname(toRGB(pars$colour))
    
    # remove names (JSON file was getting confused)
    pars <- lapply(pars, unname)
  }
  
  ## x and y locations
  if(major) {
    pars$loc$x <- as.list(built$panel$ranges[[1]]$x.major_source)
    pars$loc$y <- as.list(built$panel$ranges[[1]]$y.major_source)
  } else {
    pars$loc$x <- as.list(built$panel$ranges[[1]]$x.minor_source)
    pars$loc$y <- as.list(built$panel$ranges[[1]]$y.minor_source)
    ## remove minor lines when major lines are already drawn
    pars$loc$x <- pars$loc$x[
      !(pars$loc$x %in% plot.meta$grid_major$loc$x)
      ]
    pars$loc$y <- pars$loc$y[
      !(pars$loc$y %in% plot.meta$grid_major$loc$y)
      ]
  }
  pars
}


#' Get unique axis labels for the plot
#' @param plot.meta contains axis labels
#' @return modified \code{plot.meta} with unique axis labels
getUniqueAxisLabels <- function(plot.meta){
  axis.info <- plot.meta[grepl("^axis[0-9]+$", names(plot.meta))]
  plot.meta$xlabs <- as.list(unique(unlist(lapply(axis.info, "[", "xlab"))))
  plot.meta$ylabs <- as.list(unique(unlist(lapply(axis.info, "[", "ylab"))))
  return(plot.meta)
}


getPlotTitle <- function(plot.title, meta.title){
  if("a_element_blank"%in%attr(plot.title, "class")){
    return("")
  } else {
    return(meta.title)
  }
}


getWidthAndHeight <- function(a_theme){
  options_list <- list()
  for(wh in c("width", "height")){
    awh <- paste0("animint.", wh)
    options_list[[wh]] <- if(awh %in% names(a_theme)){
      a_theme[[awh]]
    }else{
      400
    }
  }
  options_list
}


setUpdateAxes <- function(a_theme, options_list){
  update_axes <- "animint.update_axes"
  if(update_axes %in% names(a_theme)){
    options_list$update_axes <- a_theme[[update_axes]]
  }
  options_list
}


hjust2anchor <- function(hjust){
  if(is.null(hjust))return(NULL)
  stopifnot(is.numeric(hjust))
  trans <-
    c("0"="start",
      "0.5"="middle",
      "1"="end")
  hjust.str <- as.character(hjust)
  is.valid <- hjust.str %in% names(trans)
  if(all(is.valid)){
    ## as.character removes names.
    as.character(trans[hjust.str])
  }else{
    print(hjust[!is.valid])
    stop("animint only supports hjust values 0, 0.5, 1")
  }
}


#' Get all parameters for a layer
#' 
#' @param l A single layer of the plot
#' @return All parameters in the layer
getLayerParams <- function(l){
  params <- c(l$a_geom_params, l$a_stat_params, l$a_aes_params, l$extra_params)
  if("chunk_vars" %in% names(params) && is.null(params[["chunk_vars"]])){
    params[["chunk_vars"]] <- character()
  }
  for(p.name in names(params)){
    names(params[[p.name]]) <- NULL
    ## Ignore functions.
    if(is.function(params[[p.name]])){
      params[[p.name]] <- NULL
    }
  }
  params
}


#' Filter out columns that do not need to be copied
#' 
#' @param g Geom with columns
#' @param s.a_aes Selector aesthetics
#' @return Character vector of columns not to be copied
colsNotToCopy <- function(g, s.a_aes){
  group.not.specified <- ! "group" %in% names(g$a_aes)
  n.groups <- length(unique(NULL))
  need.group <- c("violin", "step", "hex")
  group.meaningless <- g$a_geom %in% c(
    "abline", "blank",
    ##"crossbar", "pointrange", #documented as unsupported
    ## "rug", "dotplot", "quantile", "smooth", "boxplot",
    ## "bin2d", "map"
    "errorbar", "errorbarh",
    ##"bar", "histogram", #?
    "hline", "vline",
    "jitter", "linerange",
    "point", 
    "rect", "segment")
  dont.need.group <- ! g$a_geom %in% need.group
  remove.group <- group.meaningless ||
    group.not.specified && 1 < n.groups && dont.need.group
  do.not.copy <- c(
    if(remove.group)"group")
  
  do.not.copy
}


## Generate error for non-Identity a_stat + showSelected
checkForNonIdentityAndSS <- function(a_stat.type, has.show, is.show, l,
                                     g_classed, g_data_names,
                                     a_aes_names){
  if(has.show && a_stat.type != "a_StatIdentity"){
    show.names <- a_aes_names[is.show]
    data.has.show <- show.names %in% g_data_names
    signal <- if(all(data.has.show))warning else stop
    print(l)
    signal(
      "showSelected does not work with ",
      a_stat.type,
      ", problem: ",
      g_classed)
  }
}


#' Flip axes in case of a_coord_flip
#' Switches column names. Eg. xmin to ymin, yntercept to xintercept etc.
#' @param col.names Column names which need to be switched
#' @return Column names with x and y axes switched
switch_axes <- function(col.names){
  for(elem in seq_along(col.names)){
    if(grepl("^x", col.names[elem])){
      col.names[elem] <- sub("^x", "y", col.names[elem])
    } else if(grepl("^y", col.names[elem])){
      col.names[elem] <- sub("^y", "x", col.names[elem])
    }
  }
  col.names
}


## Check if output type is linetype
is.linetype <- function(x){
  x <- tolower(x)
  namedlinetype <-
    x%in%c("blank", "solid", "dashed",
           "dotted", "dotdash", "longdash", "twodash")
  xsplit <- sapply(x, function(i){
    sum(is.na(strtoi(strsplit(i,"")[[1]],16)))==0
  })
  namedlinetype | xsplit
}


## Remove PANEL column from data if it has a single unique value
removeUniquePanelValue <- function(g.data, plot.has.panels){
  PANEL_vals <- unique(g.data[["PANEL"]])
  a_geom.has.one.panel <- length(PANEL_vals) == 1
  if(a_geom.has.one.panel && (!plot.has.panels)) {
    g.data <- g.data[names(g.data) != "PANEL"]
  }
  g.data
}


#' Check plot.list for errors
#' 
#' Check that plot.list is a list and every a_element is named
#' @param plot.list from \code{animint2dir} to check for errors
#' @return Throws an error for invalid values
checkPlotList <- function(plot.list){
  if (!is.list(plot.list))
    stop("plot.list must be a list of ggplots")
  if (is.null(names(plot.list)))
    stop("plot.list must be a named list")
  if (any(names(plot.list)==""))
    stop("plot.list must have names with non-empty strings")
  return(NULL)
}


#' Environment to store meta data
#' 
#' Get a new environment to store meta-data. Used to alter state in the
#' lower-level functions
#' @return A new environment to store meta data
newEnvironment <- function(){
  meta <- new.env()
  meta$plots <- list()
  meta$geoms <- list()
  meta$selectors <- list()
  return(meta)
}


#' Check animation variable for errors
#' 
#' @param timeVarList \code{plot.list$time} in \code{animint2dir} to check
#' for errors
#' @return \code{NULL} :Stops with an error for invalid input 
checkAnimationTimeVar <- function(timeVarList){
  # Check if both ms duration and variable are present
  if(!all(c("ms", "variable") %in% names(timeVarList))){
    stop("time option list needs ms, variable")
  }
  
  ms <- timeVarList$ms
  stopifnot(is.numeric(ms))
  stopifnot(length(ms)==1)
  ## NOTE: although we do not use plot.list$ms for anything in the R
  ## code, it is used to control the number of milliseconds between
  ## animation frames in the JS code.
  timeVar <- timeVarList$variable
  stopifnot(is.character(timeVar))
  stopifnot(length(timeVar)==1)
  return(NULL)
}

#' Performs error checking on the plot for animint extensions
#' 
#' @param p plot from \code{plot.list} to check for errors
#' @param plot_name plot name error check. Should be alphanumeric and should
#' begin with an alphabet
#' @return \code{NULL} :Stops with an error for invalid input 
checkPlotForAnimintExtensions <- function(p, plot_name){
  # Check if plot is properly named
  pattern <- "^[a-zA-Z][a-zA-Z0-9]*$"
  if(!grepl(pattern, plot_name)){
    stop("ggplot names must match ", pattern)
  }
  
  # Check layer by layer for proper extensions
  for(L in p$layers){
    ## This code assumes that the layer has the complete aesthetic
    ## mapping and data. TODO: Do we need to copy any global
    ## values to this layer?
    name.counts <- table(names(L$orig_mapping))
    is.dup <- 1 < name.counts
    if(any(is.dup)){
      print(L)
      stop("aes names must be unique, problems: ",
           paste(names(name.counts)[is.dup], collapse=", "))
    }
    
    ## Add SS and CS as aesthetics before checking for interactive aes
    ## TODO: We are doing this twice. Once in parsePlot too. Restructure
    ## to avoid this
    a_aesthetics_added <- addSSandCSasAesthetics(L$mapping, L$extra_params)
    iaes <- selectSSandCS(a_aesthetics_added)
    one.names <- with(iaes, c(clickSelects$one, showSelected$one))
    update.vars <- as.character(a_aesthetics_added[one.names])
    # if the layer has a defined data set
    if(length(L$data) > 0) {
      # check whether the variable is in that layer
      has.var <- update.vars %in% names(L$data)
    } else {
      # check whether the variable is in the global data
      has.var <- update.vars %in% names(p$data)
    }
    
    if(!all(has.var)){
      print(L)
      print(list(problem.a_aes=update.vars[!has.var],
                 data.variables=names(L$data)))
      stop("data does not have interactive variables")
    }
    has.cs <- !is.null(L$extra_params$clickSelects)
    has.href <- "href" %in% names(L$mapping)
    if(has.cs && has.href){
      stop("clickSelects can not be used with aes(href)")
    }
  }
  return(NULL)
}


## Compute domains of different subsets, to be used by update_scales
## in the renderer
compute_domains <- function(built_data, axes, geom_name,
                            vars, split_by_panel, mapping){
  names_present <- names(built_data) %in% mapping
  ## Map variables to column names
  return_names <- function(name_selectors, mapping){
    for(i in seq_along(mapping)){
      if(mapping[[i]] == name_selectors){
        return(names(mapping)[[i]])
      }
    }
  }
  
  names(built_data)[names_present] <- sapply(names(built_data)[names_present], return_names, rev(mapping))
  # Different geoms will use diff columns to calculate domains for
  # showSelected subsets. Eg. a_geom_bar will use 'xmin', 'xmax', 'ymin',
  # 'ymax' etc. while a_geom_point will use 'x', 'y'
  domain_cols <- list(bar=c(paste0(axes, "min"), paste0(axes, "max")),
                      ribbon=if(axes=="x"){c(axes)}
                      else{c(paste0(axes, "min"), paste0(axes, "max"))},
                      rect=c(paste0(axes, "min"), paste0(axes, "max")),
                      tallrect=if(axes=="x")
                      {c(paste0("xmin"), paste0("xmax"))}
                      else{NULL},
                      point=c(axes),
                      path=c(axes),
                      text=c(axes),
                      line=c(axes),
                      segment=c(axes, paste0(axes, "end")))
  use_cols <- domain_cols[[geom_name]]
  if(is.null(use_cols)){
    warning(paste0("axis updates have not yet been implemented for a_geom_",
                   geom_name), call. = FALSE)
    return(NULL)
  }else if(!all(use_cols %in% names(built_data))){
    return(NULL)
  }
  domain_vals <- list()
  inter_data <- built_data[[ vars[[1]] ]]
  # If we have more than one showSelected vars, we need to compute
  # every possible subset domain
  if(length(vars) > 1){
    for(i in 2:length(vars)){
      inter_data <- interaction(inter_data, built_data[[vars [[i]] ]],
                                sep = "_")
    }
  }
  # Split by PANEL only when specified, else use first value of PANEL
  # It is a hack and must be handled in a better way
  split_by <- if(split_by_panel){
    interaction(built_data$PANEL, inter_data)
  }else{
    levels(inter_data) <- paste0(unique(built_data$PANEL[[1]]),
                                 ".", levels(inter_data))
    inter_data
  }
  
  if(geom_name %in% c("point", "path", "text", "line")){
    # We suppress 'returning Inf' warnings when we compute a factor
    # interaction that has no data to display
    domain_vals[[use_cols[1]]] <- 
      suppressWarnings(lapply(split(built_data[[use_cols[1]]],
                                    split_by),
                              range, na.rm=TRUE))
  }else if(geom_name %in% c("bar", "rect", "tallrect")){
    # Calculate min and max values of each subset separately
    min_vals <- suppressWarnings(lapply(split(built_data[[use_cols[1]]],
                                              split_by),
                                        min, na.rm=TRUE))
    max_vals <- suppressWarnings(lapply(split(built_data[[use_cols[2]]],
                                              split_by),
                                        max, na.rm=TRUE))
    domain_vals <- list(mapply(c, min_vals, max_vals, SIMPLIFY = FALSE))
  }else if(geom_name %in% c("segment")){
    domain_vals[[use_cols[1]]] <-
      suppressWarnings(lapply(split(built_data[, use_cols], split_by),
                              range, na.rm=TRUE))
  }else if(geom_name %in% c("ribbon")){
    if(axes=="x"){
      domain_vals[[use_cols[1]]] <- 
        suppressWarnings(lapply(split(built_data[[use_cols[1]]],
                                      split_by),
                                range, na.rm=TRUE))
    }else{
      min_vals <- suppressWarnings(lapply(split(built_data[[use_cols[1]]],
                                                split_by),
                                          min, na.rm=TRUE))
      max_vals <- suppressWarnings(lapply(split(built_data[[use_cols[2]]],
                                                split_by),
                                          max, na.rm=TRUE))
      domain_vals <- list(mapply(c, min_vals, max_vals, SIMPLIFY = FALSE))
    }
  }
  domain_vals
}

## Out of all the possible geoms, get the min/max value which will
## determine the domain to be used in the renderer
get_domain <- function(subset_domains){
  use_domain <- list()
  ## ggplot gives a margin of 5% at all four sides which does not
  ## have any plotted data. So axis ranges are 10% bigger than the
  ## actual ranges of data. We do the same here
  extra_margin = 0.05
  for(i in unique(unlist(lapply(subset_domains, names)))){
    all_vals <- lapply(subset_domains, "[[", i)
    all_vals <- all_vals[!sapply(all_vals, is.null)]
    min_val <- min(sapply(all_vals, "[[", 1))
    max_val <- max(sapply(all_vals, "[[", 2))
    # We ignore non finite values that may have creeped in while
    # calculating all possible subset domains
    if(all(is.finite(c(max_val, min_val)))){
      use_domain[[i]] <-if(max_val - min_val > 0){
        c(min_val - (extra_margin *(max_val-min_val)),
          max_val + (extra_margin *(max_val-min_val)))
      }else{
        # If min_val and max_val are same, return a range equal to
        # the value
        warning("some data subsets have only a single data value to plot",
                call. = FALSE)
        return_dom <- c(min_val - (0.5 * min_val), max_val + (0.5 * max_val))
        if(min_val == 0){
          # if min_val = max_val = 0, return a range (-1, 1)
          return_dom <- c(-1, 1)
        }
        return_dom
      }
    }else{
      warning("some data subsets have no data to plot", call. = FALSE)
    }
  }
  use_domain
}

## get axis ticks and major/minor grid lines for updating plots
get_ticks_gridlines <- function(use_domain){
  gridlines <- list()
  for (i in seq_along(use_domain)){
    all_lines <- scales::pretty_breaks(n=10)(use_domain[[i]])
    if(length(all_lines) > 0){
      # make sure grid lines are not outside plot domain
      if(use_domain[[i]][1] > all_lines[[1]]){
        all_lines <- all_lines[2:length(all_lines)]
      }
      if(use_domain[[i]][2] < all_lines[[length(all_lines)]]){
        all_lines <- all_lines[1:(length(all_lines)-1)]
      }
      # Every second grid line is minor, rest major
      # Major grid lines are also used for drawing axis ticks
      # Eg. If all_lines = 1:10
      # minor grid lines = 1, 3, 5, 7, 9
      # major grid lines = 2, 4, 6, 8, 10
      majors <- all_lines[c(FALSE, TRUE)]
      minors <- all_lines[c(TRUE, FALSE)]
      gridlines[[ names(use_domain)[[i]] ]] <- list(minors, majors)
    }
  }
  gridlines
}


#' Issue warnings for selectors
#' @param geoms \code{geoms} to check for warnings
#' @param selector.a_aes selectors for each geom
#' @param duration animation variable information to check for \code{key} value
#' @return \code{NULL}
issueSelectorWarnings <- function(geoms, selector.a_aes, duration){
  for(g.name in names(geoms)){
    g.info <- geoms[[g.name]]
    g.selectors <- selector.a_aes[[g.name]]
    show.vars <- g.info$a_aes[g.selectors$showSelected$one]
    duration.vars <- names(duration)
    show.with.duration <- show.vars[show.vars %in% duration.vars]
    no.key <- ! "key" %in% names(g.info$a_aes)
    if(length(show.with.duration) && no.key){
      warning(
        "to ensure that smooth transitions are interpretable, ",
        "aes(key) should be specifed for geoms with showSelected=",
        show.with.duration[1],
        ", problem: ", g.name)
    }
  }
  return(NULL)
}

#' Gives a unique name to each layer in \code{saveLayer}
#' @param L layer in saveLayer to be named
#' @param geom_num the number of the layer to be saved
#' @param p.name the name of the plot to which the layer belongs
#' @return a unique name for the layer
getLayerName <- function(L, geom_num, p.name){
  # carson's approach to getting layer types
  ggtype <- function (x, y = "a_geom") {
    sub(y, "", tolower(class(x[[y]])[1]))
  }
  a_layer_name <- sprintf("a_geom%d_%s_%s",
                        geom_num, ggtype(L), p.name)
  a_layer_name
}


#' Issue warnings for non interactive plots where there is only one
#' showSelected value
#' @param selectors selectors to check for warnings
#' @return \code{NULL}
checkSingleShowSelectedValue <- function(selectors){
  if(length(selectors) > 0){
    n.levels <- sapply(selectors, function(s.info)length(s.info$levels))
    one.level <- n.levels == 1
    has.legend <- sapply(selectors, function(s.info)isTRUE(s.info$legend))
    is.trivial <- one.level & (!has.legend)
    if(any(is.trivial)){
      ## With the current compiler that has already saved the tsv files
      ## by now, we can't really make this data viz more efficient by
      ## ignoring this trivial selector. However we can warn the user so
      ## that they can remove this inefficient showSelected.
      warning("showSelected variables with only 1 level: ",
              paste(names(selectors)[is.trivial], collapse=", "))
    }
  }
  return(NULL)
}


#' Set plot width and height for all plots
#' @param meta meta object with all information
#' @param AllPlotsInfo ....
#' @return \code{NULL}. Sizes are stored in meta object
setPlotSizes <- function(meta, AllPlotsInfo){
  # Set both width and height
  for(d in c("width","height")){
    size <- meta[[d]]
    if(is.list(size)){
      warning("option ", d, " is deprecated, ",
              "use a_plot()+a_theme_animint(", d,
              "=", size[[1]],
              ") instead")
      if(is.null(names(size))){ #use this size for all plots.
        for(plot.name in names(AllPlotsInfo)){
          AllPlotsInfo[[plot.name]]$options[[d]] <- size[[1]]
        }
      }else{ #use the size specified for the named plot.
        for(plot.name in names(size)){
          if(plot.name %in% names(AllPlotsInfo)){
            AllPlotsInfo[[plot.name]]$options[[d]] <- size[[plot.name]]
          }else{
            stop("no ggplot named ", plot.name)
          }
        }
      }
    }
  }
  return(NULL)
}



#' Merge a list of data frames.
#' @param dfs list of data frames
#' @return data frame
merge_recurse <- function(dfs){
  a_label.vec <- unique(unlist(lapply(dfs, function(df)paste(df$a_label))))
  result <- data.frame(row.names=a_label.vec)
  for(df in dfs){
    df.a_label <- paste(df$a_label)
    for(col.name in names(df)){
      result[df.a_label, col.name] <- df[[col.name]]
    }
  }
  result
}


#' Function to get legend information for each scale
#' @param mb single entry from a_guides_merge() list of legend data
#' @return list of legend information, NULL if a_guide=FALSE.
getLegend <- function(mb){
  a_guidetype <- mb$name
  
  ## The main idea of legends:
  
  ## 1. Here in getLegend I export the legend entries as a list of
  ## rows that can be used in a data() bind in D3.
  
  ## 2. In add_legend in the JS code I create a <table> for every
  ## legend, and then I bind the legend entries to <tr>, <td>, and
  ## <svg> elements.
  cleanData <- function(data, key, a_geom, params) {
    nd <- nrow(data)
    nk <- nrow(key)
    if (nd == 0) return(data.frame()); # if no rows, return an empty df.
    if ("a_guide" %in% names(params)) {
      if (params[["a_guide"]] == "none") return(data.frame()); # if no a_guide, return an empty df
    }
    if (nd != nk) warning("key and data have different number of rows")
    if (!".a_label" %in% names(key)) return(data.frame()); # if there are no a_labels, return an empty df.
    data$`.a_label` <- key$`.a_label`
    data <- data[, which(colSums(!is.na(data)) > 0)] # remove cols that are entirely na
    if("colour" %in% names(data)) data[["colour"]] <- toRGB(data[["colour"]]) # color hex values
    if("fill" %in% names(data)) data[["fill"]] <- toRGB(data[["fill"]]) # fill hex values
    names(data) <- paste0(a_geom, names(data))# a_aesthetics by a_geom
    names(data) <- gsub(paste0(a_geom, "."), "", names(data), fixed=TRUE) # a_label isn't geom-specific
    data$a_label <- paste(data$a_label) # otherwise it is AsIs.
    data
  }
  dataframes <- mapply(function(i, j) cleanData(i$data, mb$key, j, i$params),
                       mb$geoms, mb$a_geom.legend.list, SIMPLIFY = FALSE)
  dataframes <- dataframes[which(sapply(dataframes, nrow)>0)]
  # Check to make sure datframes is non-empty. If it is empty, return NULL.
  if(length(dataframes)>0) {
    data <- merge_recurse(dataframes)
  } else return(NULL)
  a_label.num <- suppressWarnings({
    as.numeric(data$a_label)
  })
  ## mb$breaks could be a vector of values to use, NULL, or an empty
  ## list with class "waiver"
  breaks.specified <- length(mb$breaks)
  entry.order <- if(breaks.specified || anyNA(a_label.num)){
    1:nrow(data)
  }else{
    nrow(data):1
  }
  data <- lapply(entry.order, function(i) as.list(data[i,]))
  if(a_guidetype=="none"){
    NULL
  }else{
    list(a_guide = a_guidetype,
         geoms = unlist(mb$a_geom.legend.list),
         title = mb$title,
         class = if(mb$is.discrete)mb$selector else mb$title,
         selector = mb$selector,
         is_discrete= mb$is.discrete,
         legend_type = mb$legend_type, 
         entries = data)
  }
}


##' Save the common columns for each tsv to one chunk
##' @param built data.frame of built data.
##' @param vars character vector of chunk variable names to split on.
##' @param a_aes.list a character vector of a_aesthetics.
##' @return a list of common and varied data to save, or NULL if there is
##' no common data.
getCommonChunk <- function(built, chunk.vars, a_aes.list){
  if(length(chunk.vars) == 0){
    return(NULL)
  }
  if(! "group" %in% names(a_aes.list)){
    ## user did not specify group, so do not use any ggplot2-computed
    ## group for deciding common data.
    built$group <- NULL
  }
  
  ## Remove columns with all NA values
  ## so that common.not.na is not empty
  ## due to the plot's alpha, stroke or other columns
  all.nas <- sapply(built, function(x){all(is.na(x))})
  built <- built[, !all.nas]
  
  ## Treat factors as characters, to avoid having them be coerced to
  ## integer later.
  for(col.name in names(built)){
    if(is.factor(built[, col.name])){
      built[, col.name] <- paste(built[, col.name])
    }
  }
  
  ## If there is only one chunk, then there is no point of making a
  ## common data file.
  chunk.rows.tab <- table(built[, chunk.vars])
  if(length(chunk.rows.tab) == 1) return(NULL)
  
  ## If there is no group column, and all the chunks are the same
  ## size, then add one based on the row number.
  if(! "group" %in% names(built)){
    chunk.rows <- chunk.rows.tab[1]
    same.size <- chunk.rows == chunk.rows.tab
    order.args <- lapply(chunk.vars, function(order.col)built[[order.col]])
    built <- built[do.call(order, order.args),]
    if(all(same.size)){
      built$group <- 1:chunk.rows
    }else{
      ## do not save a common chunk file.
      return(NULL)
    }
  }
  
  built.by.group <- split(built, built$group)
  group.tab <- table(built[, c("group", chunk.vars)])
  each.group.same.size <- apply(group.tab, 1, function(group.size.vec){
    group.size <- group.size.vec[1]
    if(all(group.size == group.size.vec)){
      ## groups are all this size.
      group.size
    }else{
      ## groups not the same size.
      0
    }
  })
  
  checkCommon <- function(col.name){
    for(group.name in names(built.by.group)){
      data.vec <- built.by.group[[group.name]][[col.name]]
      if(group.size <- each.group.same.size[[group.name]]){
        not.same.value <- data.vec != data.vec[1:group.size]
        if(any(not.same.value, na.rm=TRUE)){
          ## if any data values are different, then this is not a
          ## common column.
          return(FALSE)
        }
      }else{
        ## this group has different sizes in different chunks, so the
        ## only way that we can make common data is if there is only
        ## value.
        value.tab <- table(data.vec)
        if(length(value.tab) != 1){
          return(FALSE)
        }
      }
    }
    TRUE
  }
  
  all.col.names <- names(built)
  col.name.vec <- all.col.names[!all.col.names %in% chunk.vars]
  is.common <- sapply(col.name.vec, checkCommon)
  
  ## TODO: another criterion could be used to save disk space even if
  ## there is only 1 chunk.
  n.common <- sum(is.common)
  if(is.common[["group"]] && 2 <= n.common && n.common < length(is.common)){
    common.cols <- names(is.common)[is.common]
    group.info.list <- list()
    for(group.name in names(built.by.group)){
      one.group <- built.by.group[[group.name]]
      group.size <- each.group.same.size[[group.name]]
      if(group.size == 0){
        group.size <- 1
      }
      group.common <- one.group[, common.cols]
      ## Instead of just taking the first chunk for this group (which
      ## may have NA), look for the chunk which has the fewest NA.
      is.na.vec <- apply(is.na(group.common), 1, any)
      is.na.mat <- matrix(is.na.vec, group.size)
      group.i <- which.min(colSums(is.na.mat))
      offset <- (group.i-1)*group.size
      group.info.list[[group.name]] <- group.common[(1:group.size)+offset, ]
    }
    group.info.common <- do.call(rbind, group.info.list)
    common.unique <- unique(group.info.common)
    ## For a_geom_polygon and a_geom_path we may have two rows that should
    ## both be kept (the start and the end of each group may be the
    ## same if the shape is closed), so we define common.data as all
    ## of the rows (common.not.na) in that case, and just the unique
    ## data per group (common.unique) in the other case.
    data.per.group <- table(common.unique$group)
    common.data <- if(all(data.per.group == 1)){
      common.unique
    }else{
      group.info.common
    }
    varied.df.list <- split.x(na.omit(built), chunk.vars)
    varied.cols <- c("group", names(is.common)[!is.common])
    varied.data <- varied.chunk(varied.df.list, varied.cols)
    return(list(common=na.omit(common.data),
                varied=varied.data))
  }
}


##' Extract subset for each data.frame in a list of data.frame
##' @param df.or.list a data.frame or a list of data.frame.
##' @param cols cols that each data.frame would keep.
##' @return list of data.frame.
varied.chunk <- function(df.or.list, cols){
  if(is.data.frame(df.or.list)){
    df <- df.or.list[, cols, drop = FALSE]
    u.df <- unique(df)
    group.counts <- table(u.df$group)
    if(all(group.counts == 1)){
      u.df
    }else{
      df
    }
  } else{
    lapply(df.or.list, varied.chunk, cols)
  }
}


##' Split data.frame into recursive list of data.frame.
##' @param x data.frame.
##' @param vars character vector of variable names to split on.
##' @return recursive list of data.frame.
split.x <- function(x, vars){
  if(length(vars)==0)return(x)
  if(is.data.frame(x)){
    
    ## Remove columns with all NA values
    ## so that x is not empty due to
    ## the plot's alpha, stroke or other columns
    all.nas <- sapply(x, function(col.m){all(is.na(col.m))})
    x <- x[, !all.nas]
    
    # rows with NA should not be saved
    x <- na.omit(x)
    if(length(vars) == 1){
      split(x[names(x) != vars], x[vars], drop = TRUE)
    }else{
      use <- vars[1]
      rest <- vars[-1]
      df.list <- split(x[names(x) != use], x[use], drop = TRUE)
      split.x(df.list, rest)
    }
  }else if(is.list(x)){
    lapply(x, split.x, vars)
  }else{
    str(x)
    stop("unknown object")
  }
}


##' Split data set into chunks and save them to separate files.
##' @param x data.frame.
##' @param meta environment.
##' @return recursive list of chunk file names.
##' @author Toby Dylan Hocking
saveChunks <- function(x, meta){
  if(is.data.frame(x)){
    this.i <- meta$chunk.i
    csv.name <- sprintf("%s_chunk%d.tsv", meta$g$classed, this.i)
    write.table(x, file.path(meta$out.dir, csv.name), quote=FALSE, 
                row.names=FALSE, sep="\t")
    meta$chunk.i <- meta$chunk.i + 1L
    this.i
  }else if(is.list(x)){
    lapply(x, saveChunks, meta)
  }else{
    str(x)
    stop("unknown object")
  }
}


##' Check if showSelected and clickSelects have been used as a_aesthetics
##' as in old syntax. If yes, raise error
##' @param a_aesthetics list. aesthetics mapping of the layer
##' @param plot_name character vector of the plot the layer belongs to
##' @return \code{NULL} Throws error if used as aesthetics
checkForSSandCSasAesthetics <- function(a_aesthetics, plot_name){
  for(i in seq_along(a_aesthetics)){
    a_aes_has_ss_cs <- grepl("^showSelected", names(a_aesthetics)[[i]]) ||
      grepl("^clickSelects$", names(a_aesthetics)[[i]])
    
    ## Show error only if showSelected is not added by animint code for legends
    ## TODO: Better check this before adding showSelectedlegend...
    ss_added_by_legend <- grepl("^showSelectedlegend", names(a_aesthetics)[[i]])
    if(a_aes_has_ss_cs && !ss_added_by_legend){
      stop(paste("Use of clickSelects and showSelected as",
                 "aesthetics has been deprecated. Please use",
                 "as parameters. Problem:", "\nPlot: ",
                 plot_name), call. = FALSE)
    }
  }
  return(NULL)
}


##' Add the showSelected/clickSelects params to the aesthetics mapping
##' @param a_aesthetics list. Original aesthetics mapping of the layer
##' @param extra_params named list containing the details of showSelected
##' and clickSelects values of the layer
##' @return Modified aesthetics list with showSelected/clickSelects params added
##' @details Used before calling ggplot_build in parsePlot and while checking
##' animint extensions to raise error 
addSSandCSasAesthetics <- function(a_aesthetics, extra_params){
  for(i in seq_along(extra_params)){
    if(names(extra_params)[[i]] == "showSelected"){
      if(is.null(names(extra_params[[i]]))){
        names(extra_params[[i]]) <- 
          rep("", length(extra_params[[i]]))
      }
      for(j in seq_along(extra_params[[i]])){
        
        ## If .variable/.value have been specified
        if(names(extra_params[[i]])[[j]] != ""){
          a_aesthetics[[length(a_aesthetics)+1]] <-
            as.symbol(names(extra_params[[i]])[[j]])
          names(a_aesthetics)[[length(a_aesthetics)]] <-
            paste0("showSelected.variable")
          a_aesthetics[[length(a_aesthetics)+1]] <-
            as.symbol(extra_params[[i]][[j]])
          names(a_aesthetics)[[length(a_aesthetics)]] <-
            paste0("showSelected.value")
        }else{
          ss_added_by_legend <- a_aesthetics[ grepl("^showSelectedlegend", names(a_aesthetics)) ]
          if(!extra_params[[i]][[j]] %in% ss_added_by_legend){
            a_aesthetics[[length(a_aesthetics)+1]] <- as.symbol(extra_params[[i]][[j]])
            names(a_aesthetics)[[length(a_aesthetics)]] <-
              paste0("showSelected", j)
          }
        }
      }
    }
    
    if(names(extra_params)[[i]] == "clickSelects"){
      if(is.null(names(extra_params[[i]]))){
        names(extra_params[[i]]) <- 
          rep("", length(extra_params[[i]]))
      }
      for(j in seq_along(extra_params[[i]])){
        if(names(extra_params[[i]])[[j]] != ""){
          a_aesthetics[[length(a_aesthetics)+1]] <-
            as.symbol(names(extra_params[[i]])[[j]])
          names(a_aesthetics)[[length(a_aesthetics)]] <-
            paste0("clickSelects.variable")
          a_aesthetics[[length(a_aesthetics)+1]] <-
            as.symbol(extra_params[[i]][[j]])
          names(a_aesthetics)[[length(a_aesthetics)]] <-
            paste0("clickSelects.value")
        }else{
          a_aesthetics[[length(a_aesthetics)+1]] <- as.symbol(extra_params[[i]][[j]])
          names(a_aesthetics)[[length(a_aesthetics)]] <-
            paste0("clickSelects")
        }
      }
    }
  }
  return(a_aesthetics)
}

##' Check \code{extra_params} argument for duplicates, non-named list
##' @param extra_params named list containing the details of showSelected
##' and clickSelects values of the layer
##' @param a_aes_mapping aesthetics mapping of the layer
##' @return Modified \code{extra_params} list
checkExtraParams <- function(extra_params, a_aes_mapping){
  for(i in seq_along(extra_params)){
    if(names(extra_params)[[i]] %in% c("showSelected", "clickSelects")){
      if(is.null(names(extra_params[[i]]))){
        ## If showSelected/clickSelects is not a named vector (due to no SSvar=SSval),
        ## just put empty strings as names
        names(extra_params[[i]]) <- 
          rep("", length(extra_params[[i]]))
      }
      ## Remove duplicates
      extra_params[[i]] <- extra_params[[i]][ !duplicated(extra_params[[i]]) ]
      
      ## Remove from extra_params if already added by legend
      if(names(extra_params)[[i]] %in% c("showSelected")){
        ss_added_by_legend <- a_aes_mapping[grepl("^showSelectedlegend", names(a_aes_mapping))]
        extra_params[[i]] <- extra_params[[i]][ !extra_params[[i]] %in% ss_added_by_legend ]
      }
    }
  }
  return(extra_params)
}

##' Separate .variable/.value selectors
##' @param a_aesthetics_list aesthetics mapping of the layer
##' @return Modified \code{a_aes.list} list with separated
##' showSelected.variable/value
selectSSandCS <- function(a_aesthetics_list){
  a_aes.list <- list(showSelected=list(one=NULL, several=data.frame()),
                   clickSelects=list(one=NULL, several=data.frame()))
  for(i in seq_along(a_aesthetics_list)){
    if(names(a_aesthetics_list)[[i]] == "showSelected.variable"){
      a_aes.list$showSelected$several <- data.frame(variable="showSelected.variable",
                                                  value="showSelected.value")
    }else if(grepl("^showSelected", names(a_aesthetics_list)[[i]]) &&
             !grepl("^showSelected.value", names(a_aesthetics_list)[[i]])){
      a_aes.list$showSelected$one <- c(a_aes.list$showSelected$one,
                                       names(a_aesthetics_list)[[i]])
    }else if(names(a_aesthetics_list)[[i]] == "clickSelects.variable"){
      a_aes.list$clickSelects$several <- data.frame(variable="clickSelects.variable",
                                                  value="clickSelects.value")
    }else if(grepl("^clickSelects", names(a_aesthetics_list)[[i]])&&
             !grepl("^clickSelects.value", names(a_aesthetics_list)[[i]])){
      a_aes.list$clickSelects$one <- c(a_aes.list$clickSelects$one,
                                     names(a_aesthetics_list)[[i]])
    }
  }
  ## TODO: how to handle showSelected$ignored in prev animint code??
  return(a_aes.list)
}
