#' Empirical Cumulative Density Function
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If \code{TRUE}, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' }
#' @export
#' @examples
#' \donttest{
#' df <- data.frame(x = rnorm(1000))
#' a_plot(df, a_aes(x)) + a_stat_ecdf(a_geom = "step")
#'
#' df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'                  g = gl(2, 100))
#'
#' a_plot(df, a_aes(x, colour = g)) + a_stat_ecdf()
#' }
a_stat_ecdf <- function(mapping = NULL, data = NULL,
                      a_geom = "step", a_position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatEcdf,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatEcdf <- a_ggproto("a_StatEcdf", a_Stat,
  compute_group = function(data, scales, n = NULL, pad = TRUE) {
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }

    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    y <- ecdf(data$x)(x)

    data.frame(x = x, y = y)
  },

  default_aes = a_aes(y = ..y..),

  required_aes = c("x")
)

