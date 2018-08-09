#' Plot data ellipses.
#'
#' The method for calculating the ellipses has been modified from
#' \code{car::ellipse} (Fox and Weisberg, 2011)
#'
#' @references John Fox and Sanford Weisberg (2011). An {R} Companion to
#'   Applied Regression, Second Edition. Thousand Oaks CA: Sage. URL:
#'   \url{http://socserv.socsci.mcmaster.ca/jfox/Books/Companion}
#' @param level The confidence level at which to draw an ellipse (default is 0.95),
#'   or, if \code{type="euclid"}, the radius of the circle to be drawn.
#' @param type The type of ellipse.
#'   The default \code{"t"} assumes a multivariate t-distribution, and
#'   \code{"norm"} assumes a multivariate normal distribution.
#'   \code{"euclid"} draws a circle with the radius equal to \code{level},
#'   representing the euclidean distance from the center.
#'   This ellipse probably won't appear circular unless \code{a_coord_fixed()} is applied.
#' @param segments The number of segments to be used in drawing the ellipse.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' a_plot(faithful, a_aes(waiting, eruptions)) +
#'   a_geom_point() +
#'   a_stat_ellipse()
#'
#' a_plot(faithful, a_aes(waiting, eruptions, color = eruptions > 3)) +
#'   a_geom_point() +
#'   a_stat_ellipse()
#'
#' a_plot(faithful, a_aes(waiting, eruptions, color = eruptions > 3)) +
#'   a_geom_point() +
#'   a_stat_ellipse(type = "norm", linetype = 2) +
#'   a_stat_ellipse(type = "t")
#'
#' a_plot(faithful, a_aes(waiting, eruptions, color = eruptions > 3)) +
#'   a_geom_point() +
#'   a_stat_ellipse(type = "norm", linetype = 2) +
#'   a_stat_ellipse(type = "euclid", level = 3) +
#'   a_coord_fixed()
#'
#' a_plot(faithful, a_aes(waiting, eruptions, fill = eruptions > 3)) +
#'   a_stat_ellipse(a_geom = "polygon")
a_stat_ellipse <- function(mapping = NULL, data = NULL,
                         a_geom = "path", a_position = "identity",
                         ...,
                         type = "t",
                         level = 0.95,
                         segments = 51,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatEllipse,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      type = type,
      level = level,
      segments = segments,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_StatEllipse <- a_ggproto("a_StatEllipse", a_Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, type = "t", level = 0.95,
                           segments = 51, na.rm = FALSE) {
    calculate_ellipse(data = data, vars = c("x", "y"), type = type,
                      level = level, segments = segments)
  }
)

calculate_ellipse <- function(data, vars, type, level, segments){
  dfn <- 2
  dfd <- nrow(data) - 1

  if (!type %in% c("t", "norm", "euclid")) {
    message("Unrecognized ellipse type")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else if (dfd < 3) {
    message("Too few points to calculate an ellipse")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else {
    if (type == "t") {
      v <- MASS::cov.trob(data[,vars])
    } else if (type == "norm") {
      v <- stats::cov.wt(data[,vars])
    } else if (type == "euclid") {
      v <- stats::cov.wt(data[,vars])
      v$cov <- diag(rep(min(diag(v$cov)), 2))
    }
    shape <- v$cov
    center <- v$center
    chol_decomp <- chol(shape)
    if (type == "euclid") {
      radius <- level/max(chol_decomp)
    } else {
      radius <- sqrt(dfn * stats::qf(level, dfn, dfd))
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  }

  ellipse <- as.data.frame(ellipse)
  colnames(ellipse) <- vars
  ellipse
}
