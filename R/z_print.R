##' Print animint by rendering to local directory.
##' @export
##' @title print animint
##' @param x List of ggplots and options. In particular the out.dir
##'   option is passed along to animint2dir.
##' @param ... passed to animint2dir
##' @return same as animint2dir
##' @author Toby Dylan Hocking
print.animint <- function(x, ...){
  if(is.null(x$out.dir)){
    message('Saving animint in temporary directory; specify output directory using animint(out.dir="path/to/directory")')
  }
  animint2dir(x, x$out.dir, ...)
}

##' Create an animated, interactive data visualization. The easiest
##' way to get started is by reading the Animint2 Manual,
##' https://animint-manual-en.netlify.app
##' @details This function creates a list with the items in ... and
##'   attaches the animint class. It also provides default names for
##'   un-named ggplots. The list should contain ggplots and
##'   options. Each geom can be made interactive by using the 
##'   showSelected and clickSelects parameters; each should be a
##'   character vector of selection variable names. For example
##'   geom_line(clickSelects="country") means that clicking the line
##'   changes the value of the "country" selection variable;
##'   geom_point(showSelected="year") means to only show the subset of
##'   data for the currently selected year.
##' @export
##' @title Create an animint
##' @param ... ggplots and options
##' @return list of class animint
##' @author Toby Dylan Hocking
##' @example inst/examples/animint.R
animint <- function(...){
  L <- list(...)
  # Check if argument list is empty
  if(length(L) == 0) {
    stop("No arguments passed to animint. Arguments should include ggplots(1 or more) and options(0 or more)")
  }
  plot.num.vec <- paste0("plot", seq(1, length(L)*2))
  default.name.vec <- plot.num.vec[!plot.num.vec %in% names(L)][1:length(L)]
  if(is.null(names(L))){
    names(L) <- default.name.vec
  }
  still.empty <- is.na(names(L)) | names(L)==""
  names(L)[still.empty] <- default.name.vec[still.empty]
  name.tab <- table(names(L))
  rep.names <- names(name.tab)[1 < name.tab]
  if(length(rep.names)){
    stop("Duplicate named arguments are passed to animint. Duplicate argument names found: ", paste(rep.names, collapse=","))
  }
  for(list.name in names(L)){
    p <- L[[list.name]]
    if(is.ggplot(p)){
      for(layer_i in seq_along(p$layers)){
        layer <- p$layers[[layer_i]]
        if(inherits(layer$geom, "GeomPoint")){
          if(!is.null(layer$aes_params$shape) && is.numeric(layer$aes_params$shape) && layer$aes_params$shape != 21){
            warning(sprintf("animint2 web rendering only supports shape=21, but shape=%s was specified", layer$aes_params$shape), call.=FALSE)
          }
          if("shape" %in% names(layer$mapping)){
            warning("animint2 web rendering only supports shape=21. Mapping shape aesthetic may not render correctly", call.=FALSE)
          }
        }
      }
    }
  }
  structure(L, class="animint")
}

