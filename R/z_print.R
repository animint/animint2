##' Print animint by rendering to local directory.
##' @export
##' @title print animint
##' @param x List of ggplots and options
##' @param ... passed to animint2dir
##' @return same as animint2dir
##' @author Toby Dylan Hocking
print.animint <- function(x, ...){
  if(is.null(x$out.dir)){
    message('Saving animint in temporary directory; specify output directory using animint(out.dir="path/to/directory")')
  }
  animint2dir(x, x$out.dir, ...)
}

##' Create an animated, interactive data visualization. This function
##' creates a list with the items in ... and attaches the animint
##' class. It also provides default names for un-named ggplots. The
##' easiest way to learn is by reading the Animint2 Manual,
##' http://members.cbio.mines-paristech.fr/~thocking/animint2-manual/Ch02-ggplot2.html
##' @export
##' @title Create an animint
##' @param ... ggplots and options
##' @return list of class animint
##' @author Toby Dylan Hocking
##' @examples
##' library(animint2)
##' data(WorldBank, package="animint2")
##' years <- unique(WorldBank[, "year", drop=FALSE])
##' y1960 <- subset(WorldBank, year==1960)
##' animint(
##'   ## options specify viz title, time=animation variable,
##'   ## duration=smooth transitions, multiple selection (selector.types),
##'   ## first selection.
##'   title="Linked scatterplot and time series",
##'   time=list(variable="year",ms=3000),
##'   duration=list(year=1000),
##'   selector.types=list(country="multiple"),
##'   first=list(
##'     country=c("Canada", "Japan"),
##'     year=1970),
##'   ## ggplots are rendered together for an interactive data viz.
##'   ts=ggplot()+
##'     theme_animint(width=500)+
##'     make_tallrect(WorldBank, "year")+
##'     geom_text(aes(
##'       year, life.expectancy, label=country),
##'       showSelected="country",
##'       clickSelects="country",
##'       hjust=1,
##'       data=y1960)+
##'     scale_x_continuous(limits=c(1950, NA))+
##'     geom_line(aes(
##'       year, life.expectancy, group=country, color=region),
##'       clickSelects="country",
##'       data=WorldBank,
##'       size=4,
##'       alpha=0.55),
##'   scatter=ggplot()+
##'     geom_point(aes(
##'       fertility.rate, life.expectancy,
##'       key=country, colour=region, size=population),
##'       showSelected="year",
##'       clickSelects="country",
##'       data=WorldBank)+
##'     geom_text(aes(
##'       fertility.rate, life.expectancy,
##'       key=country,
##'       label=country),
##'       showSelected=c("country", "year"),
##'       data=WorldBank)+
##'     geom_text(aes(
##'       5, 80, key=1, label=paste("year =", year)),
##'       showSelected="year",
##'       data=years)+
##'     scale_size_animint(pixel.range=c(2,20), breaks=10^(4:9)))
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

