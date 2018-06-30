#' Benchmark plot creation time.
#' Broken down into construct, build, render and draw times.
#'
#' @param x code to create ggplot2 plot
#' @keywords internal
#' @examples
#' animint2:::benchplot(a_plot(mtcars, aes(mpg, wt)) + geom_point())
#' animint2:::benchplot(a_plot(mtcars, aes(mpg, wt)) + geom_point() +
#' animint2:::a_facet_grid(. ~ cyl))
benchplot <- function(x) {

  construct <- system.time(force(x))
  stopifnot(inherits(x, "a_plot"))

  build <- system.time(data <- a_plot_build(x))
  render <- system.time(grob <- a_plot_gtable(data))
  draw <- system.time(grid.draw(grob))

  times <- rbind(construct, build, render, draw)[, 1:3]

  plyr::unrowname(data.frame(
    step = c("construct", "build", "render", "draw", "TOTAL"),
    rbind(times, colSums(times))))
}
