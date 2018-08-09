#' Fortify method for map objects.
#'
#' This function turns a map into a data frame that can more easily be
#' plotted with ggplot2.
#'
#' @export
#' @seealso \code{\link{map_data}} and \code{\link{borders}}
#' @param model map object
#' @param data not used by this method
#' @param ... not used by this method
#' @examples
#' if (require("maps")) {
#' ca <- map("county", "ca", plot = FALSE, fill = TRUE)
#' head(a_fortify(ca))
#' a_plot(ca, a_aes(long, lat)) +
#'   a_geom_polygon(a_aes(group = group))
#'
#' tx <- map("county", "texas", plot = FALSE, fill = TRUE)
#' head(a_fortify(tx))
#' a_plot(tx, a_aes(long, lat)) +
#'   a_geom_polygon(a_aes(group = group), colour = "white")
#' }
a_fortify.map <- function(model, data, ...) {
  df <- as.data.frame(model[c("x", "y")])
  names(df) <- c("long", "lat")
  df$group <- cumsum(is.na(df$long) & is.na(df$lat)) + 1
  df$order <- 1:nrow(df)

  names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df[stats::complete.cases(df$lat, df$long), ]
}

#' Create a data frame of map data.
#'
#' @param map name of map provided by the \pkg{maps} package.  These
#'   include \code{\link[maps]{county}}, \code{\link[maps]{france}},
#'   \code{\link[maps]{italy}}, \code{\link[maps]{nz}},
#'   \code{\link[maps]{state}}, \code{\link[maps]{usa}},
#'   \code{\link[maps]{world}}, \code{\link[maps]{world2}}.
#' @param region name of subregions to include.  Defaults to \code{.} which
#'   includes all subregion.  See documentation for \code{\link[maps]{map}}
#'   for more details.
#' @param exact should the \code{region} be treated as a regular expression
#'   (\code{FALSE}) or as a fixed string (\code{TRUE}).
#' @param ... all other arguments passed on to \code{\link[maps]{map}}
#' @export
#' @examples
#' if (require("maps")) {
#' states <- map_data("state")
#' arrests <- USArrests
#' names(arrests) <- tolower(names(arrests))
#' arrests$region <- tolower(rownames(USArrests))
#'
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' choro <- choro[order(choro$order), ]
#' a_plot(choro, a_aes(long, lat)) +
#'   a_geom_polygon(a_aes(group = group, fill = assault)) +
#'   a_coord_map("albers",  at0 = 45.5, lat1 = 29.5)
#'
#' a_plot(choro, a_aes(long, lat)) +
#'   a_geom_polygon(a_aes(group = group, fill = assault / murder)) +
#'   a_coord_map("albers",  at0 = 45.5, lat1 = 29.5)
#' }
map_data <- function(map, region = ".", exact = FALSE, ...) {
  try_require("maps", "map_data")
  a_fortify(map(map, region, exact = exact, plot = FALSE, fill = TRUE, ...))
}

#' Create a layer of map borders.
#'
#' @param database map data, see \code{\link[maps]{map}} for details
#' @param regions map region
#' @param fill fill colour
#' @param colour border colour
#' @param xlim,ylim latitudinal and logitudinal range for extracting map
#'   polygons, see \code{\link[maps]{map}} for details.
#' @param ... other arguments passed onto \code{\link{a_geom_polygon}}
#' @export
#' @examples
#' if (require("maps")) {
#'
#' ia <- map_data("county", "iowa")
#' mid_range <- function(x) mean(range(x))
#' seats <- plyr::ddply(ia, "subregion", plyr::colwise(mid_range, c("lat", "long")))
#' a_plot(ia, a_aes(long, lat)) +
#'   a_geom_polygon(a_aes(group = group), fill = NA, colour = "grey60") +
#'   a_geom_text(a_aes(label = subregion), data = seats, size = 2, angle = 45)
#'
#' data(us.cities)
#' capitals <- subset(us.cities, capital == 2)
#' a_plot(capitals, a_aes(long, lat)) +
#'   borders("state") +
#'   a_geom_point(a_aes(size = pop)) +
#'   a_scale_size_area() +
#'   a_coord_quickmap()
#'
#' # Same map, with some world context
#' a_plot(capitals, a_aes(long, lat)) +
#'   borders("world", xlim = c(-130, -60), ylim = c(20, 50)) +
#'   a_geom_point(a_aes(size = pop)) +
#'   a_scale_size_area() +
#'   a_coord_quickmap()
#' }
borders <- function(database = "world", regions = ".", fill = NA,
                    colour = "grey50", xlim = NULL, ylim = NULL, ...) {
  df <- map_data(database, regions, xlim = xlim, ylim = ylim)
  a_geom_polygon(a_aes_(~long, ~lat, group = ~group), data = df,
    fill = fill, colour = colour, ..., inherit.a_aes = FALSE)
}
