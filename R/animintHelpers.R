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
