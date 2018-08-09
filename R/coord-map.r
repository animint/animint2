#' Map projections.
#'
#' The representation of a portion of the earth, which is approximately spherical,
#' onto a flat 2D plane requires a projection. This is what
#' \code{\link{a_coord_map}} does. These projections account for the fact that the
#' actual length (in km) of one degree of longitude varies between the equator
#' and the pole. Near the equator, the ratio between the lengths of one degree
#' of latitude and one degree of longitude is approximately 1. Near the pole, it
#' is tends towards infinity because the length of one degree of longitude tends
#' towards 0. For regions that span only a few degrees and are not too close to
#' the poles, setting the aspect ratio of the plot to the appropriate lat/lon
#' ratio approximates the usual mercator projection. This is what
#' \code{a_coord_quickmap} does. With \code{\link{a_coord_map}} all elements of the
#' graphic have to be projected which is not the case here. So
#' \code{\link{a_coord_quickmap}} has the advantage of being much faster, in
#' particular for complex plots such as those using with
#' \code{\link{a_geom_tile}}, at the expense of correctness in the projection.
#' This coordinate system provides the full range of map projections available
#' in the mapproj package.
#'
#' @export
#' @param projection projection to use, see
#'    \code{\link[mapproj]{mapproject}} for list
#' @param ... other arguments passed on to
#'   \code{\link[mapproj]{mapproject}}
#' @param orientation projection orientation, which defaults to
#'  \code{c(90, 0, mean(range(x)))}.  This is not optimal for many
#'  projections, so you will have to supply your own. See
#'  \code{\link[mapproj]{mapproject}} for more information.
#' @param xlim manually specific x limits (in degrees of longitude)
#' @param ylim manually specific y limits (in degrees of latitude)
#' @export
#' @examples
#' if (require("maps")) {
#' nz <- map_data("nz")
#' # Prepare a map of NZ
#' nzmap <- a_plot(nz, a_aes(x = long, y = lat, group = group)) +
#'   a_geom_polygon(fill = "white", colour = "black")
#'
#' # Plot it in cartesian coordinates
#' nzmap
#' # With correct mercator projection
#' nzmap + a_coord_map()
#' # With the aspect ratio approximation
#' nzmap + a_coord_quickmap()
#'
#' # Other projections
#' nzmap + a_coord_map("cylindrical")
#' nzmap + a_coord_map("azequalarea", orientation = c(-36.92,174.6,0))
#'
#' states <- map_data("state")
#' usamap <- a_plot(states, a_aes(long, lat, group = group)) +
#'   a_geom_polygon(fill = "white", colour = "black")
#'
#' # Use cartesian coordinates
#' usamap
#' # With mercator projection
#' usamap + a_coord_map()
#' usamap + a_coord_quickmap()
#' # See ?mapproject for coordinate systems and their parameters
#' usamap + a_coord_map("gilbert")
#' usamap + a_coord_map("lagrange")
#'
#' # For most projections, you'll need to set the orientation yourself
#' # as the automatic selection done by mapproject is not available to
#' # ggplot
#' usamap + a_coord_map("orthographic")
#' usamap + a_coord_map("stereographic")
#' usamap + a_coord_map("conic", lat0 = 30)
#' usamap + a_coord_map("bonne", lat0 = 50)
#'
#' # World map, using a_geom_path instead of a_geom_polygon
#' world <- map_data("world")
#' worldmap <- a_plot(world, a_aes(x = long, y = lat, group = group)) +
#'   a_geom_path() +
#'   a_scale_y_continuous(breaks = (-2:2) * 30) +
#'   a_scale_x_continuous(breaks = (-4:4) * 45)
#'
#' # Orthographic projection with default orientation (looking down at North pole)
#' worldmap + a_coord_map("ortho")
#' # Looking up up at South Pole
#' worldmap + a_coord_map("ortho", orientation = c(-90, 0, 0))
#' # Centered on New York (currently has issues with closing polygons)
#' worldmap + a_coord_map("ortho", orientation = c(41, -74, 0))
#' }
a_coord_map <- function(projection="mercator", ..., orientation = NULL, xlim = NULL, ylim = NULL) {
  a_ggproto(NULL, a_CoordMap,
    projection = projection,
    orientation = orientation,
    limits = list(x = xlim, y = ylim),
    params = list(...)
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_CoordMap <- a_ggproto("a_CoordMap", a_Coord,

  transform = function(self, data, scale_details) {
    trans <- mproject(self, data$x, data$y, scale_details$orientation)
    out <- cunion(trans[c("x", "y")], data)

    out$x <- rescale(out$x, 0:1, scale_details$x.proj)
    out$y <- rescale(out$y, 0:1, scale_details$y.proj)
    out
  },

  distance = function(x, y, scale_details) {
    max_dist <- dist_central_angle(scale_details$x.range, scale_details$y.range)
    dist_central_angle(x, y) / max_dist
  },

  aspect = function(ranges) {
    diff(ranges$y.proj) / diff(ranges$x.proj)
  },

  train = function(self, scale_details) {

    # range in scale
    ranges <- list()
    for (n in c("x", "y")) {

      a_scale <- scale_details[[n]]
      limits <- self$limits[[n]]

      if (is.null(limits)) {
        range <- a_scale$dimension(expand_default(a_scale))
      } else {
        range <- range(a_scale$transform(limits))
      }
      ranges[[n]] <- range
    }

    orientation <- self$orientation %||% c(90, 0, mean(ranges$x))

    # Increase chances of creating valid boundary region
    grid <- expand.grid(
      x = seq(ranges$x[1], ranges$x[2], length.out = 50),
      y = seq(ranges$y[1], ranges$y[2], length.out = 50)
    )

    ret <- list(x = list(), y = list())

    # range in map
    proj <- mproject(self, grid$x, grid$y, orientation)$range
    ret$x$proj <- proj[1:2]
    ret$y$proj <- proj[3:4]

    for (n in c("x", "y")) {
      out <- scale_details[[n]]$break_info(ranges[[n]])
      ret[[n]]$range <- out$range
      ret[[n]]$major <- out$major_source
      ret[[n]]$minor <- out$minor_source
      ret[[n]]$a_labels <- out$a_labels
    }

    details <- list(
      orientation = orientation,
      x.range = ret$x$range, y.range = ret$y$range,
      x.proj = ret$x$proj, y.proj = ret$y$proj,
      x.major = ret$x$major, x.minor = ret$x$minor, x.a_labels = ret$x$a_labels,
      y.major = ret$y$major, y.minor = ret$y$minor, y.a_labels = ret$y$a_labels
    )
    details
  },

  render_bg = function(self, scale_details, a_theme) {
    xrange <- expand_range(scale_details$x.range, 0.2)
    yrange <- expand_range(scale_details$y.range, 0.2)

    # Limit ranges so that lines don't wrap around globe
    xmid <- mean(xrange)
    ymid <- mean(yrange)
    xrange[xrange < xmid - 180] <- xmid - 180
    xrange[xrange > xmid + 180] <- xmid + 180
    yrange[yrange < ymid - 90] <- ymid - 90
    yrange[yrange > ymid + 90] <- ymid + 90

    xgrid <- with(scale_details, expand.grid(
      y = c(seq(yrange[1], yrange[2], length.out = 50), NA),
      x = x.major
    ))
    ygrid <- with(scale_details, expand.grid(
      x = c(seq(xrange[1], xrange[2], length.out = 50), NA),
      y = y.major
    ))

    xlines <- self$transform(xgrid, scale_details)
    ylines <- self$transform(ygrid, scale_details)

    if (nrow(xlines) > 0) {
      grob.xlines <- a_element_render(
        a_theme, "panel.grid.major.x",
        xlines$x, xlines$y, default.units = "native"
      )
    } else {
      grob.xlines <- a_zeroGrob()
    }

    if (nrow(ylines) > 0) {
      grob.ylines <- a_element_render(
        a_theme, "panel.grid.major.y",
        ylines$x, ylines$y, default.units = "native"
      )
    } else {
      grob.ylines <- a_zeroGrob()
    }

    ggname("grill", grobTree(
      a_element_render(a_theme, "panel.background"),
      grob.xlines, grob.ylines
    ))
  },

  render_axis_h = function(self, scale_details, a_theme) {
    if (is.null(scale_details$x.major)) return(a_zeroGrob())

    x_intercept <- with(scale_details, data.frame(
      x = x.major,
      y = y.range[1]
    ))
    pos <- self$transform(x_intercept, scale_details)

    a_guide_axis(pos$x, scale_details$x.a_labels, "bottom", a_theme)
  },

  render_axis_v = function(self, scale_details, a_theme) {
    if (is.null(scale_details$y.major)) return(a_zeroGrob())

    x_intercept <- with(scale_details, data.frame(
      x = x.range[1],
      y = y.major
    ))
    pos <- self$transform(x_intercept, scale_details)

    a_guide_axis(pos$y, scale_details$y.a_labels, "left", a_theme)
  }
)


mproject <- function(a_coord, x, y, orientation) {
  suppressWarnings(mapproj::mapproject(x, y,
    projection = a_coord$projection,
    parameters  = a_coord$params,
    orientation = orientation
  ))
}
