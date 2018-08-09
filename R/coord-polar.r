#' Polar coordinates.
#'
#' The polar coordinate system is most commonly used for pie charts, which
#' are a stacked bar chart in polar coordinates.
#'
#' @param theta variable to map angle to (\code{x} or \code{y})
#' @param start offset of starting point from 12 o'clock in radians
#' @param direction 1, clockwise; -1, anticlockwise
#' @export
#' @examples
#' # NOTE: Use these plots with caution - polar coordinates has
#' # major perceptual problems.  The main point of these examples is
#' # to demonstrate how these common plots can be described in the
#' # grammar.  Use with EXTREME caution.
#'
#' #' # A pie chart = stacked bar chart + polar coordinates
#' pie <- a_plot(mtcars, a_aes(x = factor(1), fill = factor(cyl))) +
#'  a_geom_bar(width = 1)
#' pie + a_coord_polar(theta = "y")
#'
#' \donttest{
#'
#' # A coxcomb plot = bar chart + polar coordinates
#' cxc <- a_plot(mtcars, a_aes(x = factor(cyl))) +
#'   a_geom_bar(width = 1, colour = "black")
#' cxc + a_coord_polar()
#' # A new type of plot?
#' cxc + a_coord_polar(theta = "y")
#'
#' # The bullseye chart
#' pie + a_coord_polar()
#'
#' # Hadley's favourite pie chart
#' df <- data.frame(
#'   variable = c("does not resemble", "resembles"),
#'   value = c(20, 80)
#' )
#' a_plot(df, a_aes(x = "", y = value, fill = variable)) +
#'   a_geom_bar(width = 1, a_stat = "identity") +
#'   a_scale_fill_manual(values = c("red", "yellow")) +
#'   a_coord_polar("y", start = pi / 3) +
#'   labs(title = "Pac man")
#'
#' # Windrose + doughnut plot
#' if (require("ggplot2movies")) {
#' movies$rrating <- cut_interval(movies$rating, length = 1)
#' movies$budgetq <- cut_number(movies$budget, 4)
#'
#' doh <- a_plot(movies, a_aes(x = rrating, fill = budgetq))
#'
#' # Wind rose
#' doh + a_geom_bar(width = 1) + a_coord_polar()
#' # Race track plot
#' doh + a_geom_bar(width = 0.9, position = "fill") + a_coord_polar(theta = "y")
#' }
#' }
a_coord_polar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  a_ggproto(NULL, a_CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction)
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_CoordPolar <- a_ggproto("a_CoordPolar", a_Coord,

  aspect = function(details) 1,

  distance = function(self, x, y, details) {
    if (self$theta == "x") {
      r <- rescale(y, from = details$r.range)
      theta <- theta_rescale_no_clip(self, x, details)
    } else {
      r <- rescale(x, from = details$r.range)
      theta <- theta_rescale_no_clip(self, y, details)
    }

    dist_polar(r, theta)
  },

  range = function(self, scale_details) {
    setNames(
      list(scale_details$theta.range, scale_details$r.range),
      c(self$theta, self$r)
    )
  },

  train = function(self, scale_details) {

    ret <- list(x = list(), y = list())
    for (n in c("x", "y")) {

      a_scale <- scale_details[[n]]
      limits <- self$limits[[n]]

      if (is.null(limits)) {
        if (self$theta == n) {
          expand <- expand_default(a_scale, c(0, 0.5), c(0, 0))
        } else {
          expand <- expand_default(a_scale, c(0, 0),   c(0, 0))
        }
        range <- a_scale$dimension(expand)
      } else {
        range <- range(a_scale_transform(a_scale, limits))
      }

      out <- a_scale$break_info(range)
      ret[[n]]$range <- out$range
      ret[[n]]$major <- out$major_source
      ret[[n]]$minor <- out$minor_source
      ret[[n]]$a_labels <- out$a_labels
    }

    details = list(
      x.range = ret$x$range, y.range = ret$y$range,
      x.major = ret$x$major, x.minor = ret$x$minor, x.a_labels = ret$x$a_labels,
      y.major = ret$y$major, y.minor = ret$y$minor, y.a_labels = ret$y$a_labels
    )

    if (self$theta == "y") {
      names(details) <- gsub("x\\.", "r.", names(details))
      names(details) <- gsub("y\\.", "theta.", names(details))
    } else {
      names(details) <- gsub("x\\.", "theta.", names(details))
      names(details) <- gsub("y\\.", "r.", names(details))
    }

    details
  },

  transform = function(self, data, scale_details) {
    data <- rename_data(self, data)

    data$r  <- r_rescale(self, data$r, scale_details)
    data$theta <- theta_rescale(self, data$theta, scale_details)
    data$x <- data$r * sin(data$theta) + 0.5
    data$y <- data$r * cos(data$theta) + 0.5

    data
  },

  render_axis_v = function(self, scale_details, a_theme) {
    x <- r_rescale(self, scale_details$r.major, scale_details) + 0.5
    a_guide_axis(x, scale_details$r.a_labels, "left", a_theme)
  },

  render_axis_h = function(scale_details, a_theme) {
    a_guide_axis(NA, "", "bottom", a_theme)
  },

  render_bg = function(self, scale_details, a_theme) {
    scale_details <- rename_data(self, scale_details)

    theta <- if (length(scale_details$theta.major) > 0)
      theta_rescale(self, scale_details$theta.major, scale_details)
    thetamin <- if (length(scale_details$theta.minor) > 0)
      theta_rescale(self, scale_details$theta.minor, scale_details)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    rfine <- c(r_rescale(self, scale_details$r.major, scale_details), 0.45)

    # This gets the proper a_theme element for theta and r grid lines:
    #   panel.grid.major.x or .y
    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    ggname("grill", grobTree(
      a_element_render(a_theme, "panel.background"),
      if (length(theta) > 0) a_element_render(
        a_theme, majortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
        y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) a_element_render(
        a_theme, minortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
        y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
        id.lengths = rep(2, length(thetamin)),
        default.units = "native"
      ),

      a_element_render(
        a_theme, majorr, name = "radius",
        x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
        y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units = "native"
      )
    ))
  },

  render_fg = function(self, scale_details, a_theme) {
    if (is.null(scale_details$theta.major)) {
      return(a_element_render(a_theme, "panel.border"))
    }

    theta <- theta_rescale(self, scale_details$theta.major, scale_details)
    a_labels <- scale_details$theta.a_labels

    # Combine the two ends of the scale if they are close
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1]) %% (2 * pi)
    if (length(theta) > 0 && ends_apart < 0.05) {
      n <- length(a_labels)
      if (is.expression(a_labels)) {
        combined <- substitute(paste(a, "/", b),
          list(a = a_labels[[1]], b = a_labels[[n]]))
      } else {
        combined <- paste(a_labels[1], a_labels[n], sep = "/")
      }
      a_labels[[n]] <- combined
      a_labels <- a_labels[-1]
      theta <- theta[-1]
    }

    grobTree(
      if (length(a_labels) > 0) a_element_render(
        a_theme, "axis.text.x",
        a_labels, 0.45 * sin(theta) + 0.5, 0.45 * cos(theta) + 0.5,
        hjust = 0.5, vjust = 0.5,
        default.units = "native"
      ),
      a_element_render(a_theme, "panel.border")
    )
  },

  render_fg = function(self, scale_details, a_theme) {
    if (is.null(scale_details$theta.major)) {
      return(a_element_render(a_theme, "panel.border"))
    }

    theta <- theta_rescale(self, scale_details$theta.major, scale_details)
    a_labels <- scale_details$theta.a_labels

    # Combine the two ends of the scale if they are close
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1]) %% (2*pi)
    if (length(theta) > 0 && ends_apart < 0.05) {
      n <- length(a_labels)
      if (is.expression(a_labels)) {
        combined <- substitute(paste(a, "/", b),
          list(a = a_labels[[1]], b = a_labels[[n]]))
      } else {
        combined <- paste(a_labels[1], a_labels[n], sep = "/")
      }
      a_labels[[n]] <- combined
      a_labels <- a_labels[-1]
      theta <- theta[-1]
    }

    grobTree(
      if (length(a_labels) > 0) a_element_render(
        a_theme, "axis.text.x",
        a_labels,
        unit(0.45 * sin(theta) + 0.5, "native"),
        unit(0.45 * cos(theta) + 0.5, "native"),
        hjust = 0.5, vjust = 0.5
      ),
      a_element_render(a_theme, "panel.border")
    )
  },

  a_labels = function(self, scale_details) {
    if (self$theta == "y") {
      list(x = scale_details$y, y = scale_details$x)
    } else {
      scale_details
    }
  }
)


rename_data <- function(a_coord, data) {
  if (a_coord$theta == "y") {
    plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
  } else {
    plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
  }
}

theta_rescale_no_clip <- function(a_coord, x, scale_details) {
  rotate <- function(x) (x + a_coord$start) * a_coord$direction
  rotate(rescale(x, c(0, 2 * pi), scale_details$theta.range))
}

theta_rescale <- function(a_coord, x, scale_details) {
  rotate <- function(x) (x + a_coord$start) %% (2 * pi) * a_coord$direction
  rotate(rescale(x, c(0, 2 * pi), scale_details$theta.range))
}

r_rescale <- function(a_coord, x, scale_details) {
  rescale(x, c(0, 0.4), scale_details$r.range)
}
