#' Ribbons and area plots.
#'
#' For each continuous x value, \code{a_geom_interval} displays a y interval.
#' \code{a_geom_area} is a special case of \code{a_geom_ribbon}, where the
#' minimum of the range is fixed to 0.
#'
#' An area plot is the continuous analog of a stacked bar chart (see
#' \code{\link{a_geom_bar}}), and can be used to show how composition of the
#' whole varies over the range of x.  Choosing the order in which different
#' components is stacked is very important, as it becomes increasing hard to
#' see the individual pattern as you move up the stack.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "ribbon")}
#'
#' @seealso
#'   \code{\link{a_geom_bar}} for discrete intervals (bars),
#'   \code{\link{a_geom_linerange}} for discrete intervals (lines),
#'   \code{\link{a_geom_polygon}} for general polygons
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @export
#' @examples
#' # Generate data
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' h <- a_plot(huron, a_aes(year))
#'
#' h + a_geom_ribbon(a_aes(ymin=0, ymax=level))
#' h + a_geom_area(a_aes(y = level))
#'
#' # Add a_aesthetic mappings
#' h +
#'   a_geom_ribbon(a_aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
#'   a_geom_line(a_aes(y = level))
a_geom_ribbon <- function(mapping = NULL, data = NULL,
                        a_stat = "identity", a_position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomRibbon,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomRibbon <- a_ggproto("a_GeomRibbon", a_Geom,
  default_aes = a_aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "ymin", "ymax"),

  draw_key = a_draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_group = function(data, panel_scales, a_coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group, data$x), ]

    # Check that aesthetics are constant
    a_aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(a_aes) > 1) {
      stop("Aesthetics can not vary with a ribbon")
    }
    a_aes <- as.list(a_aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    positions <- plyr::summarise(data,
      x = c(x, rev(x)), y = c(ymax, rev(ymin)), id = c(ids, rev(ids)))
    munched <- a_coord_munch(a_coord, positions, panel_scales)

    ggname("geom_ribbon", polygonGrob(
      munched$x, munched$y, id = munched$id,
      default.units = "native",
      gp = gpar(
        fill = alpha(a_aes$fill, a_aes$alpha),
        col = a_aes$colour,
        lwd = a_aes$size * .pt,
        lty = a_aes$linetype)
    ))
  }
)

#' @rdname a_geom_ribbon
#' @export
a_geom_area <- function(mapping = NULL, data = NULL, a_stat = "identity",
                      a_position = "stack", na.rm = FALSE, show.legend = NA,
                      inherit.a_aes = TRUE, ...) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomArea,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomArea <- a_ggproto("a_GeomArea", a_GeomRibbon,
  default_aes = a_aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    transform(data, ymin = 0, ymax = y)
  }
)
