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
##' https://rcdata.nau.edu/genomic-ml/animint2-manual/Ch02-ggplot2.html
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
##' @examples
##' if(require('data.table'))setDTthreads(1)#for CRAN.
##' library(animint2)
##' data(WorldBank, package="animint2")
##' years <- unique(WorldBank[, "year", drop=FALSE])
##' y1960 <- subset(WorldBank, year==1960)
##' animint(
##'   title="Linked scatterplot and time series", #web page title.
##'   time=list(variable="year",ms=3000), #variable and time delay used for animation.
##'   duration=list(year=1000), #smooth transition duration in milliseconds.
##'   selector.types=list(country="multiple"), #single/multiple selection for each variable.
##'   first=list( #selected values to show when viz is first rendered.
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
  structure(L, class="animint")
}

