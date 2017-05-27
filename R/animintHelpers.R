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
    layer.has.variable <- s.name %in% names(L$data)
    
    if(is.variable.name && layer.has.variable) {
      ## grabbing the variable from the data
      var <- L$data[, s.name]
      is.interactive.aes <-
        grepl("showSelected|clickSelects", names(L$mapping))
      is.legend.var <- L$mapping == s.name
      ## If s.name is used with another interactive aes, then do
      ## not add any showSelected aesthetic for it.
      var.is.interactive <- any(is.interactive.aes & is.legend.var)
      if(!var.is.interactive){
        ## only add showSelected aesthetic if the variable is
        ## used by the geom
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


## extract panel background and borders from theme.pars
get_bg <- function(pars, theme.pars) {
  # if pars is not an empty list - occurs when using element_blank()
  if(length(pars) > 0) {
    
    ## if elements are not specified, they inherit from theme.pars$rect
    for(i in 1:length(pars)) {
      if(is.null(pars[[i]])) pars[[i]] <- unname(theme.pars$rect[[i]])
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
get_grid <- function(pars, theme.pars, plot.meta, meta, major = T) {
  # if pars is not an empty list - occurs when using element_blank()
  if(length(pars) > 0) {
    
    ## if elements are not specified, they inherit from 
    ##    theme.pars$panel.grid then from theme.pars$line
    for(i in names(pars)) {
      if(is.null(pars[[i]])) pars[[i]] <- 
          if(!is.null(theme.pars$panel.grid[[i]])) {
            theme.pars$panel.grid[[i]]
          } else {
            theme.pars$line[[i]]
          }
    }
    # convert colour to RGB if necessary
    if(!is.rgb(pars$colour)) pars$colour <- unname(toRGB(pars$colour))
    
    # remove names (JSON file was getting confused)
    pars <- lapply(pars, unname)
  }
  
  ## x and y locations
  if(major) {
    pars$loc$x <- as.list(meta$built$panel$ranges[[1]]$x.major_source)
    pars$loc$y <- as.list(meta$built$panel$ranges[[1]]$y.major_source)
  } else {
    pars$loc$x <- as.list(meta$built$panel$ranges[[1]]$x.minor_source)
    pars$loc$y <- as.list(meta$built$panel$ranges[[1]]$y.minor_source)
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
  if("element_blank"%in%attr(plot.title, "class")){
    return("")
  } else {
    return(meta.title)
  }
}


getWidthAndHeight <- function(theme){
  options_list <- list()
  for(wh in c("width", "height")){
    awh <- paste0("animint.", wh)
    options_list[[wh]] <- if(awh %in% names(theme)){
      theme[[awh]]
    }else{
      400
    }
  }
  options_list
}


setUpdateAxes <- function(theme, options_list){
  update_axes <- "animint.update_axes"
  if(update_axes %in% names(theme)){
    options_list$update_axes <- theme[[update_axes]]
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
  params <- c(l$geom_params, l$stat_params, l$aes_params, l$extra_params)
  for(p.name in names(params)){
    if("chunk_vars" %in% names(params) && is.null(params[["chunk_vars"]])){
      params[["chunk_vars"]] <- character()
    }
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
#' @param s.aes Selector aesthetics
#' @return Character vector of columns not to be copied
colsNotToCopy <- function(g, s.aes){
  group.not.specified <- ! "group" %in% names(g$aes)
  n.groups <- length(unique(NULL))
  need.group <- c("violin", "step", "hex")
  group.meaningless <- g$geom %in% c(
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
  dont.need.group <- ! g$geom %in% need.group
  remove.group <- group.meaningless ||
    group.not.specified && 1 < n.groups && dont.need.group
  do.not.copy <- c(
    if(remove.group)"group",
    s.aes$showSelected$ignored,
    s.aes$clickSelects$ignored)
  do.not.copy
}


## Generate error for non-Identity Stat + showSelected
checkForNonIdentityAndSS <- function(stat.type, has.show, is.show, l,
                                     g_classed, g_data_names,
                                     aes_names){
  if(has.show && stat.type != "StatIdentity"){
    show.names <- aes_names[is.show]
    data.has.show <- show.names %in% names(g.data)
    signal <- if(all(data.has.show))warning else stop
    print(l)
    signal(
      "showSelected does not work with ",
      stat.type,
      ", problem: ",
      g$classed)
  }
}


#' Flip axes in case of coord_flip
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


#' Check plot.list for errors
#' 
#' Check that plot.list is a list and every element is named
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
    name.counts <- table(names(L$mapping))
    is.dup <- 1 < name.counts
    if(any(is.dup)){
      print(L)
      stop("aes names must be unique, problems: ",
           paste(names(name.counts)[is.dup], collapse=", "))
    }
    iaes <- selector.aes(L$mapping)
    one.names <- with(iaes, c(clickSelects$one, showSelected$one))
    update.vars <- as.character(L$mapping[one.names])
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
      print(list(problem.aes=update.vars[!has.var],
                 data.variables=names(L$data)))
      stop("data does not have interactive variables")
    }
    has.cs <- 0 < with(iaes$clickSelects, nrow(several) + length(one))
    has.href <- "href" %in% names(L$mapping)
    if(has.cs && has.href){
      stop("aes(clickSelects) can not be used with aes(href)")
    }
  }
  return(NULL)
}


#' Issue warnings for selectors
#' @param geoms \code{geoms} to check for warnings
#' @param selector.aes selectors for each geom
#' @param duration animation variable information to check for \code{key} value
#' @return \code{NULL}
issueSelectorWarnings <- function(geoms, selector.aes, duration){
  for(g.name in names(geoms)){
    g.info <- geoms[[g.name]]
    g.selectors <- selector.aes[[g.name]]
    show.vars <- g.info$aes[g.selectors$showSelected$one]
    duration.vars <- names(duration)
    show.with.duration <- show.vars[show.vars %in% duration.vars]
    no.key <- ! "key" %in% names(g.info$aes)
    if(length(show.with.duration) && no.key){
      warning(
        "to ensure that smooth transitions are interpretable, ",
        "aes(key) should be specifed for geoms with aes(showSelected=",
        show.with.duration[1],
        "), problem: ", g.name)
    }
  }
  return(NULL)
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
#' @return \code{NULL}. Sizes are stored in meta object
setPlotSizes <- function(meta){
  # Set both width and height
  for(d in c("width","height")){
    size <- meta[[d]]
    if(is.list(size)){
      warning("option ", d, " is deprecated, ",
              "use ggplot()+theme_animint(", d,
              "=", size[[1]],
              ") instead")
      if(is.null(names(size))){ #use this size for all plots.
        for(plot.name in names(meta$plots)){
          meta$plots[[plot.name]]$options[[d]] <- size[[1]]
        }
      }else{ #use the size specified for the named plot.
        for(plot.name in names(size)){
          if(plot.name %in% names(meta$plots)){
            meta$plots[[plot.name]]$options[[d]] <- size[[plot.name]]
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
  label.vec <- unique(unlist(lapply(dfs, function(df)paste(df$label))))
  result <- data.frame(row.names=label.vec)
  for(df in dfs){
    df.label <- paste(df$label)
    for(col.name in names(df)){
      result[df.label, col.name] <- df[[col.name]]
    }
  }
  result
}


#' Function to get legend information for each scale
#' @param mb single entry from ggplot2:::guides_merge() list of legend data
#' @return list of legend information, NULL if guide=FALSE.
getLegend <- function(mb){
  guidetype <- mb$name
  
  ## The main idea of legends:
  
  ## 1. Here in getLegend I export the legend entries as a list of
  ## rows that can be used in a data() bind in D3.
  
  ## 2. In add_legend in the JS code I create a <table> for every
  ## legend, and then I bind the legend entries to <tr>, <td>, and
  ## <svg> elements.
  cleanData <- function(data, key, geom, params) {
    nd <- nrow(data)
    nk <- nrow(key)
    if (nd == 0) return(data.frame()); # if no rows, return an empty df.
    if ("guide" %in% names(params)) {
      if (params[["guide"]] == "none") return(data.frame()); # if no guide, return an empty df
    }
    if (nd != nk) warning("key and data have different number of rows")
    if (!".label" %in% names(key)) return(data.frame()); # if there are no labels, return an empty df.
    data$`.label` <- key$`.label`
    data <- data[, which(colSums(!is.na(data)) > 0)] # remove cols that are entirely na
    if("colour" %in% names(data)) data[["colour"]] <- toRGB(data[["colour"]]) # color hex values
    if("fill" %in% names(data)) data[["fill"]] <- toRGB(data[["fill"]]) # fill hex values
    names(data) <- paste0(geom, names(data))# aesthetics by geom
    names(data) <- gsub(paste0(geom, "."), "", names(data), fixed=TRUE) # label isn't geom-specific
    data$label <- paste(data$label) # otherwise it is AsIs.
    data
  }
  dataframes <- mapply(function(i, j) cleanData(i$data, mb$key, j, i$params),
                       mb$geoms, mb$geom.legend.list, SIMPLIFY = FALSE)
  dataframes <- dataframes[which(sapply(dataframes, nrow)>0)]
  # Check to make sure datframes is non-empty. If it is empty, return NULL.
  if(length(dataframes)>0) {
    data <- merge_recurse(dataframes)
  } else return(NULL)
  label.num <- suppressWarnings({
    as.numeric(data$label)
  })
  ## mb$breaks could be a vector of values to use, NULL, or an empty
  ## list with class "waiver"
  breaks.specified <- length(mb$breaks)
  entry.order <- if(breaks.specified || anyNA(label.num)){
    1:nrow(data)
  }else{
    nrow(data):1
  }
  data <- lapply(entry.order, function(i) as.list(data[i,]))
  if(guidetype=="none"){
    NULL
  }else{
    list(guide = guidetype,
         geoms = unlist(mb$geom.legend.list),
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
##' @param aes.list a character vector of aesthetics.
##' @return a list of common and varied data to save, or NULL if there is
##' no common data.
getCommonChunk <- function(built, chunk.vars, aes.list){
  if(length(chunk.vars) == 0){
    return(NULL)
  }
  if(! "group" %in% names(aes.list)){
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
    ## For geom_polygon and geom_path we may have two rows that should
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
