#' @include geom-map.r
NULL

#' Annotation: maps.
#'
#' @param map data frame representing a map.  Most map objects can be
#'   converted into the right format by using \code{\link{a_fortify}}
#' @param ... other arguments used to modify aesthetics
#' @export
#' @examples
#' if (require("maps")) {
#' usamap <- map_data("state")
#'
#' seal.sub <- subset(seals, long > -130 & lat < 45 & lat > 40)
#' a_plot(seal.sub, a_aes(x = long, y = lat)) +
#'   a_annotation_map(usamap, fill = "NA", colour = "grey50") +
#'   a_geom_segment(a_aes(xend = long + delta_long, yend = lat + delta_lat))
#'
#' seal2 <- transform(seal.sub,
#'   latr = cut(lat, 2),
#'   longr = cut(long, 2))
#'
#' a_plot(seal2,  a_aes(x = long, y = lat)) +
#'   a_annotation_map(usamap, fill = "NA", colour = "grey50") +
#'   a_geom_segment(a_aes(xend = long + delta_long, yend = lat + delta_lat)) +
#'   a_facet_grid(latr ~ longr, scales = "free", space = "free")
#' }
a_annotation_map <- function(map, ...) {
  # Get map input into correct form
  stopifnot(is.data.frame(map))
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  stopifnot(all(c("x", "y", "id") %in% names(map)))

  a_layer(
    data = NULL,
    a_stat = a_StatIdentity,
    a_geom = a_GeomAnnotationMap,
    a_position = a_PositionIdentity,
    inherit.a_aes = FALSE,
    params = list(map = map, ...)
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomAnnotationMap <- a_ggproto("a_GeomAnnotationMap", a_GeomMap,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_scales, a_coord, map) {
    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- a_coord_munch(a_coord, map, panel_scales)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    polygonGrob(coords$x, coords$y, default.units = "native",
      id = grob_id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt)
      )
  },

  required_aes = c()
)
