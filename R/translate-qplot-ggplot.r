#' Translating between qplot and a_plot
#'
#' Within ggplot2, there are two basic methods to create plots, with qplot()
#' and a_plot(). qplot() is designed primarily for interactive use: it makes
#' a number of assumptions that speed most cases, but when designing multilayered
#' plots with different data sources it can get in the way. This section
#' describes what those defaults are, and how they map to the fuller a_plot()
#' syntax.
#'
#' @name translate_qplot_a_plot
#' @examples
#'
#' # By default, qplot() assumes that you want a scatterplot,
#' # i.e., you want to use a_geom_point()
#' # qplot(x, y, data = data)
#' # a_plot(data, a_aes(x, y)) + a_geom_point()
#'
#' # Using a_aesthetics
#'
#' # If you map additional aesthetics, these will be added to the defaults. With
#' # qplot() there is no way to use different aesthetic mappings (or data) in
#' # different layers
#' # qplot(x, y, data = data, shape = shape, colour = colour)
#' # a_plot(data, a_aes(x, y, shape = shape, colour = colour)) + a_geom_point()
#' #
#' # Aesthetic parameters in qplot() always try to map the aesthetic to a
#' # variable. If the argument is not a variable but a value, effectively a new column
#' # is added to the original dataset with that value. To set an aesthetic to a
#' # value and override the default appearance, you surround the value with I() in
#' # qplot(), or pass it as a parameter to the layer.
#' # qplot(x, y, data = data, colour = I("red"))
#' # a_plot(data, a_aes(x, y)) + a_geom_point(colour = "red")
#'
#' # Changing the a_geom parameter changes the a_geom added to the plot
#' # qplot(x, y, data = data, a_geom = "line")
#' # a_plot(data, a_aes(x, y)) + a_geom_line()
#'
#' # Not all geoms require both x and y, e.g., a_geom_bar() and a_geom_histogram().
#' # For these two geoms, if the y aesthetic is not supplied, both qplot and
#' # a_plot commands default to "count" on the y-axis
#' # a_plot(data, a_aes(x)) + a_geom_bar()
#' # qplot(x, data = data, a_geom = "bar")
#'
#' # If a vector of multiple geom names is supplied to the geom argument, each
#' # geom will be added in turn
#' # qplot(x, y, data = data, a_geom = c("point", "smooth"))
#' # a_plot(data, a_aes(x, y)) + a_geom_point() + a_geom_smooth()
#'
#' # Unlike the rest of ggplot2, stats and a_geoms are independent
#' # qplot(x, y, data = data, a_stat = "bin")
#' # a_plot(data, a_aes(x, y)) + a_geom_point(a_stat = "bin")
#' #
#' # Any layer parameters will be passed on to all layers. Most layers will ignore
#' # parameters that they don't need
#' # qplot(x, y, data = data, a_geom = c("point", "smooth"), method = "lm")
#' # a_plot(data, a_aes(x, y)) + a_geom_point(method = "lm") + a_geom_smooth(method = "lm")
#'
#' # Scales and axes
#'
#' # You can control basic properties of the x and y scales with the xlim, ylim,
#' # xlab and ylab arguments
#' # qplot(x, y, data = data, xlim = c(1, 5), xlab = "my label")
#' # a_plot(data, a_aes(x, y)) + a_geom_point() +
#' # a_scale_x_continuous("my label", limits = c(1, 5))
#'
#' # qplot(x, y, data = data, xlim = c(1, 5), ylim = c(10, 20))
#' # a_plot(data, a_aes(x, y)) + a_geom_point() +
#' # a_scale_x_continuous(limits = c(1, 5)) + a_scale_y_continuous(limits = c(10, 20))
#'
#' # Like plot(), qplot() has a convenient way of log transforming the axes.
#' # qplot(x, y, data = data, log = "xy")
#' # a_plot(data, a_aes(x, y)) + a_geom_point() + a_scale_x_log10() + a_scale_y_log10()
#' # There are many other possible transformations, but not all are
#' # accessible from within qplot(), see ?a_scale_continuous for more
#'
#' # Plot options
#'
#' # qplot() recognises the same options as plot does, and converts them to their
#' # ggplot2 equivalents. See ?a_theme for more on ggplot options
#' # qplot(x, y, data = data, main="title", asp = 1)
#' # a_plot(data, a_aes(x, y)) + a_geom_point() + labs(title = "title") + a_theme(aspect.ratio = 1)
NULL
