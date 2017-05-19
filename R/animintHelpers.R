## Animint specific helper functions

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
