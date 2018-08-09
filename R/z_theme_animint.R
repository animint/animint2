##' a_theme for passing animint specific params
##' 
##' a_theme without checks. This allows us to write
##' \code{a_theme_animint(width=500)}, instead of \code{a_theme(animint.width=500)}
##' which gives an error in ggplot2 because users should be informed
##' if they mis-type standard a_theme element
##' names. https://github.com/hadley/ggplot2/issues/938
##' @param ... a_theme options such as \code{width}. Use \code{update_axes=c("x", "y")} to update the axes of plots. Works for single selection variables.
##' @return ggplot a_theme list with names such as \code{animint.width}.
##' @examples 
##' mtcars$cyl <- as.factor(mtcars$cyl)
##' p <- a_plot() +
##'   a_geom_point(a_aes(x=wt, y=mpg, colour=cyl),
##'              data=mtcars) +
##'   ## set width and height values and update both axes
##'   a_theme_animint(width=600, height=600, update_axes=c("x", "y"))
##' viz <- list(plot=p, selector.types=list(cyl="single"))
##' animint2dir(viz)
##' @export
##' @author Toby Dylan Hocking
a_theme_animint <- function(...){
  elements <- list(...)
  names(elements) <- paste0("animint.", names(elements))
  elements$validate <- FALSE
  do.call(a_theme, elements)
}
