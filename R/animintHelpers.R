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
  meta$geom.count <- 1
  
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
