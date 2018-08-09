#' Aesthetics: group
#'
#' @name a_aes_group_order
#' @aliases group
#'
#' @examples
#' \donttest{
#'
#' # By default, the group is set to the interaction of all discrete variables in the
#' # plot. This often partitions the data correctly, but when it does not, or when
#' # no discrete variable is used in the plot, you will need to explicitly define the
#' # grouping structure, by mapping group to a variable that has a different value
#' # for each group.
#'
#' # For most applications you can simply specify the grouping with
#' # various aesthetics (colour, shape, fill, linetype) or with facets.
#'
#' p <- a_plot(mtcars, a_aes(wt, mpg))
#' # A basic scatter plot
#' p + a_geom_point(size = 4)
#' # The colour a_aesthetic
#' p + a_geom_point(a_aes(colour = factor(cyl)), size = 4)
#' # Or you can use shape to distinguish the data
#' p + a_geom_point(a_aes(shape = factor(cyl)), size = 4)
#'
#' # Using fill
#' a <- a_plot(mtcars, a_aes(factor(cyl)))
#' a + a_geom_bar()
#' a + a_geom_bar(a_aes(fill = factor(cyl)))
#' a + a_geom_bar(a_aes(fill = factor(vs)))
#'
#' # Using linetypes
#' rescale01 <- function(x) (x - min(x)) / diff(range(x))
#' ec_scaled <- data.frame(
#'   date = economics$date,
#'   plyr::colwise(rescale01)(economics[, -(1:2)]))
#' ecm <- reshape2::melt(ec_scaled, id.vars = "date")
#' f <- a_plot(ecm, a_aes(date, value))
#' f + a_geom_line(a_aes(linetype = variable))
#'
#' # Using facets
#' k <- a_plot(diamonds, a_aes(carat, ..density..)) + a_geom_histogram(binwidth = 0.2)
#' k + facet_grid(. ~ cut)
#'
#' # There are three common cases where the default is not enough, and we
#' # will consider each one below. In the following examples, we will use a simple
#' # longitudinal dataset, Oxboys, from the nlme package. It records the heights
#' # (height) and centered ages (age) of 26 boys (Subject), measured on nine
#' # occasions (Occasion).
#'
#' # Multiple groups with one aesthetic
#' h <- a_plot(nlme::Oxboys, a_aes(age, height))
#' # A single line tries to connect all the observations
#' h + a_geom_line()
#' # The group a_aesthetic maps a different line for each subject
#' h + a_geom_line(a_aes(group = Subject))
#'
#' # Different groups on different layers
#' h <- h + a_geom_line(a_aes(group = Subject))
#' # Using the group aesthetic with both a_geom_line() and a_geom_smooth()
#' # groups the data the same way for both layers
#' h + a_geom_smooth(a_aes(group = Subject), method = "lm", se = FALSE)
#' # Changing the group aesthetic for the smoother layer
#' # fits a single line of best fit across all boys
#' h + a_geom_smooth(a_aes(group = 1), size = 2, method = "lm", se = FALSE)
#'
#' # Overriding the default grouping
#' # The plot has a discrete scale but you want to draw lines that connect across
#' # groups. This is the strategy used in interaction plots, profile plots, and parallel
#' # coordinate plots, among others. For example, we draw boxplots of height at
#' # each measurement occasion
#' boysbox <- a_plot(nlme::Oxboys, a_aes(Occasion, height))
#' boysbox + a_geom_boxplot()
#' # There is no need to specify the group aesthetic here; the default grouping
#' # works because occasion is a discrete variable. To overlay individual trajectories
#' # we again need to override the default grouping for that layer with a_aes(group = Subject)
#' boysbox <- boysbox + a_geom_boxplot()
#' boysbox + a_geom_line(a_aes(group = Subject), colour = "blue")
#' }
NULL
