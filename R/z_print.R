##' Print animint by rendering to local directory.
##' @export
##' @title print animint
##' @param x List of ggplots and options
##' @param ... passed to animint2dir
##' @return same as animint2dir
##' @author Toby Dylan Hocking
print.animint <- function(x, ...){
  animint2dir(x, ...)
}

##' create a list and attach animint class
##' @export
##' @title create an animint
##' @param ... ggplots and options
##' @return list of class animint
##' @author Toby Dylan Hocking
animint <- function(...){
  L <- list(...)
  default.name.vec <- plot.num.vec <- paste0("plot", seq_along(L))
  match.name.list <- lapply(match.call()[-1], paste)
  first.name.vec <- sapply(match.name.list, "[", 1)
  sub.name.vec <- gsub("[^a-zA-Z0-9]", "", first.name.vec)
  name.ok <- grepl("^[a-zA-Z][a-zA-Z0-9]*$", sub.name.vec)
  use.name <- sapply(match.name.list, length)==1 & name.ok
  default.name.vec[use.name] <- sub.name.vec[use.name]
  if(is.null(names(L))){
    names(L) <- default.name.vec
  }
  still.empty <- names(L)==""
  names(L)[still.empty] <- default.name.vec[still.empty]
  name.tab <- table(names(L))
  is.rep <- names(L) %in% names(name.tab)[1 < name.tab]
  names(L)[is.rep] <- plot.num.vec[is.rep]
  structure(L, class="animint")
}

