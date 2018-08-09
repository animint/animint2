#' @include geom-polygon.r
NULL

#' Polygons from a reference map.
#'
#' Does not affect position scales.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "map")}
#'
#' @export
#' @param map Data frame that contains the map coordinates.  This will
#'   typically be created using \code{\link{a_fortify}} on a spatial object.
#'   It must contain columns \code{x} or \code{long}, \code{y} or
#'   \code{lat}, and \code{region} or \code{id}.
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @examples
#' # When using a_geom_polygon, you will typically need two data frames:
#' # one contains the coordinates of each polygon (positions),  and the
#' # other the values associated with each polygon (values).  An id
#' # variable links the two together
#'
#' ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
#'
#' values <- data.frame(
#'   id = ids,
#'   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
#' )
#'
#' positions <- data.frame(
#'   id = rep(ids, each = 4),
#'   x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
#'   0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
#'   y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
#'   2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
#' )
#'
#' a_plot(values) + a_geom_map(a_aes(map_id = id), map = positions) +
#'   expand_limits(positions)
#' a_plot(values, a_aes(fill = value)) +
#'   a_geom_map(a_aes(map_id = id), map = positions) +
#'   expand_limits(positions)
#' a_plot(values, a_aes(fill = value)) +
#'   a_geom_map(a_aes(map_id = id), map = positions) +
#'   expand_limits(positions) + ylim(0, 3)
#'
#' # Better example
#' crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#' crimesm <- reshape2::melt(crimes, id = 1)
#' if (require(maps)) {
#'   states_map <- map_data("state")
#'   a_plot(crimes, a_aes(map_id = state)) +
#'     a_geom_map(a_aes(fill = Murder), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat)
#'
#'   last_plot() + a_coord_map()
#'   a_plot(crimesm, a_aes(map_id = state)) +
#'     a_geom_map(a_aes(fill = value), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     a_facet_wrap( ~ variable)
#' }
a_geom_map <- function(mapping = NULL, data = NULL,
                     a_stat = "identity",
                     ...,
                     map,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.a_aes = TRUE) {
  # Get map input into correct form
  stopifnot(is.data.frame(map))
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  stopifnot(all(c("x", "y", "id") %in% names(map)))

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomMap,
    a_position = a_PositionIdentity,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      map = map,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomMap <- a_ggproto("a_GeomMap", a_GeomPolygon,
  draw_panel = function(data, panel_scales, a_coord, map) {
    # Only use matching data and map ids
    common <- intersect(data$map_id, map$id)
    data <- data[data$map_id %in% common, , drop = FALSE]
    map <- map[map$id %in% common, , drop = FALSE]

    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- a_coord_munch(a_coord, map, panel_scales)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    # Align data with map
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
    data <- data[data_rows, , drop = FALSE]

    polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt
      )
    )
  },

  required_aes = c("map_id")
)
