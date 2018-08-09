#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @inheritParams a_stat_density
#' @param a_scale if "area" (default), all violins have the same area (before trimming
#'   the tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#' @section Computed variables:
#' \describe{
#'   \item{density}{density estimate}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#'   \item{count}{density * number of points - probably useless for violin plots}
#'   \item{violinwidth}{density scaled for the violin plot, according to area, counts
#'                      or to a constant maximum width}
#'   \item{n}{number of points}
#'   \item{width}{width of violin bounding box}
#' }
#' @seealso \code{\link{a_geom_violin}} for examples, and \code{\link{a_stat_density}}
#'   for examples with data along the x axis.
#' @export
#' @rdname a_geom_violin
a_stat_ydensity <- function(mapping = NULL, data = NULL,
                          a_geom = "violin", a_position = "dodge",
                          ...,
                          bw = "nrd0",
                          adjust = 1,
                          kernel = "gaussian",
                          trim = TRUE,
                          a_scale = "area",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_scale <- match.arg(a_scale, c("area", "count", "width"))

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatYdensity,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      a_scale = a_scale,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatYdensity <- a_ggproto("a_StatYdensity", a_Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  compute_group = function(data, scales, width = NULL, bw = "nrd0", adjust = 1,
                       kernel = "gaussian", trim = TRUE, na.rm = FALSE) {
    if (nrow(data) < 3) return(data.frame())

    if (trim) {
      range <- range(data$y, na.rm = TRUE)
    } else {
      range <- scales$y$dimension()
    }
    dens <- compute_density(data$y, data$w, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel)

    dens$y <- dens$x
    dens$x <- mean(range(data$x))

    # Compute width if x has multiple values
    if (length(unique(data$x)) > 1) {
      width <- diff(range(data$x)) * 0.9
    }
    dens$width <- width

    dens
  },

  compute_panel = function(self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
                           kernel = "gaussian", trim = TRUE, na.rm = FALSE,
                           a_scale = "area") {
    data <- a_ggproto_parent(a_Stat, self)$compute_panel(
      data, scales, width = width, bw = bw, adjust = adjust, kernel = kernel,
      trim = trim, na.rm = na.rm
    )

    # choose how violins are scaled relative to each other
    data$violinwidth <- switch(a_scale,
      # area : keep the original densities but a_scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = data$density / max(data$density) * data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )
    data
  }

)
