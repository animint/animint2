#' Add a smoothed conditional mean.
#'
#' Aids the eye in seeing patterns in the presence of overplotting.
#' \code{a_geom_smooth} and \code{a_stat_smooth} are effectively aliases: they
#' both use the same arguments. Use \code{a_geom_smooth} unless you want to
#' display the results with a non-standard a_geom.
#'
#' Calculation is performed by the (currently undocumented)
#' \code{predictdf} generic and its methods.  For most methods the standard
#' error bounds are computed using the \code{\link{predict}} method - the
#' exceptions are \code{loess} which uses a t-based approximation, and
#' \code{glm} where the normal confidence interval is constructed on the link
#' scale, and then back-transformed to the response scale.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "smooth")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param a_geom,a_stat Use to override the default connection between
#'   \code{a_geom_smooth} and \code{a_stat_smooth}.
#' @seealso See individual modelling functions for more details:
#'   \code{\link{lm}} for linear smooths,
#'   \code{\link{glm}} for generalised linear smooths,
#'   \code{\link{loess}} for local smooths
#' @export
#' @examples
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_geom_smooth()
#'
#' # Use span to control the "wiggliness" of the default loess smoother
#' # The span is the fraction of points used to fit each local regression:
#' # small numbers make a wigglier curve, larger numbers make a smoother curve.
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_geom_smooth(span = 0.3)
#'
#' # Instead of a loess smooth, you can use any other modelling function:
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_geom_smooth(method = "lm", se = FALSE)
#'
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)
#'
#' # Smoothes are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet
#'
#' a_plot(mpg, a_aes(displ, hwy, colour = class)) +
#'   a_geom_point() +
#'   a_geom_smooth(se = FALSE, method = "lm")
#' a_plot(mpg, a_aes(displ, hwy)) +
#'   a_geom_point() +
#'   a_geom_smooth(span = 0.8) +
#'   a_facet_wrap(~drv)
#'
#' \donttest{
#' binomial_smooth <- function(...) {
#'   a_geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
#' }
#' # To fit a logistic regression, you need to coerce the values to
#' # a numeric vector lying between 0 and 1.
#' a_plot(rpart::kyphosis, a_aes(Age, Kyphosis)) +
#'   a_geom_jitter(height = 0.05) +
#'   binomial_smooth()
#'
#' a_plot(rpart::kyphosis, a_aes(Age, as.numeric(Kyphosis) - 1)) +
#'   a_geom_jitter(height = 0.05) +
#'   binomial_smooth()
#'
#' a_plot(rpart::kyphosis, a_aes(Age, as.numeric(Kyphosis) - 1)) +
#'   a_geom_jitter(height = 0.05) +
#'   binomial_smooth(formula = y ~ splines::ns(x, 2))
#'
#' # But in this case, it's probably better to fit the model yourself
#' # so you can exercise more control and see whether or not it's a good model
#' }
a_geom_smooth <- function(mapping = NULL, data = NULL,
                        a_stat = "smooth", a_position = "identity",
                        ...,
                        method = "auto",
                        formula = y ~ x,
                        se = TRUE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.a_aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    ...
  )
  if (identical(a_stat, "smooth")) {
    params$method <- method
    params$formula <- formula
    params$se <- se
  }

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomSmooth,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = params
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomSmooth <- a_ggproto("a_GeomSmooth", a_Geom,
  draw_group = function(data, panel_scales, a_coord) {
    ribbon <- transform(data, colour = NA)
    path <- transform(data, alpha = NA)

    has_ribbon <- !is.null(data$ymax) && !is.null(data$ymin)

    gList(
      if (has_ribbon) a_GeomRibbon$draw_group(ribbon, panel_scales, a_coord),
      a_GeomLine$draw_panel(path, panel_scales, a_coord)
    )
  },

  draw_key = a_draw_key_smooth,

  required_aes = c("x", "y"),

  default_aes = a_aes(colour = "#3366FF", fill = "grey60", size = 1,
    linetype = 1, weight = 1, alpha = 0.4)
)
