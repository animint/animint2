#' Hexagon binning.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("geom", "hex")}
#'
#' @seealso \code{\link{stat_bin2d}} for rectangular binning
#' @param geom,stat Override the default connection between \code{geom_hex} and
#'   \code{stat_binhex.}
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(carat, price))
#' d + geom_hex()
#'
#' \donttest{
#' # You can control the size of the bins by specifying the number of
#' # bins in each direction:
#' d + geom_hex(bins = 10)
#' d + geom_hex(bins = 30)
#'
#' # Or by specifying the width of the bins
#' d + geom_hex(binwidth = c(1, 1000))
#' d + geom_hex(binwidth = c(.1, 500))
#' }
geom_hex <- function(mapping = NULL, data = NULL,
                     stat = "binhex", position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHex,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-gganimintproto
#' @format NULL
#' @usage NULL
#' @export
GeomHex <- gganimintproto("GeomHex", Geom,
  draw_group = function(data, panel_scales, coord) {
    if (!inherits(coord, "CoordCartesian")) {
      stop("geom_hex() only works with Cartesian coordinates", call. = FALSE)
    }

    coord <- coord$transform(data, panel_scales)
    ggname("geom_hex", hexGrob(
      coord$x, coord$y, colour = coord$colour,
      fill = alpha(coord$fill, coord$alpha)
    ))
  },

  required_aes = c("x", "y"),

  default_aes = aes(colour = NA, fill = "grey50", size = 0.5, alpha = NA),

  draw_key = draw_key_polygon,
  pre_process = function(g, g.data) {
    g$geom <- "polygon"
    ## TODO: for interactivity we will run into the same problems as
    ## we did with histograms. Again, if we put several
    ## clickSelects/showSelected values in the same hexbin, then
    ## clicking/hiding hexbins doesn't really make sense. Need to stop
    ## with an error if showSelected/clickSelects is used with hex.
    g$aes[["group"]] <- "group"
    dx <- resolution(g.data$x, FALSE)
    dy <- resolution(g.data$y, FALSE) / sqrt(3) / 2 * 1.15
    hex <- as.data.frame(hexbin::hexcoords(dx, dy))[,1:2]
    hex <- rbind(hex, hex[1,]) # to join hexagon back to first point
    g.data$group <- as.numeric(interaction(g.data$group, 1:nrow(g.data)))
    ## this has the potential to be a bad assumption -
    ##   by default, group is identically 1, if the user
    ##   specifies group, polygons aren't possible to plot
    ##   using d3, because group will have a different meaning
    ##   than "one single polygon".
    # CPS (07-24-14) what about this? --
    # http://tdhock.github.io/animint/geoms/polygon/index.html
    newdata <- plyr::ddply(g.data, "group", function(df){
      df$xcenter <- df$x
      df$ycenter <- df$y
      cbind(x=df$x+hex$x, y=df$y+hex$y, df[,-which(names(df)%in%c("x", "y"))])
    })
    g.data <- newdata
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g.data) & "fill"%in%names(g.data)){
      g.data[["colour"]] <- g.data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g.data)) g.data[["size"]] <- 0
    }
    return(list(g = g, g.data = g.data))
  }
)


# Draw hexagon grob
# Modified from code by Nicholas Lewin-Koh and Martin Maechler
#
# @param x positions of hex centres
# @param y positions
# @param vector of hex sizes
# @param border colour
# @param fill colour
# @keyword internal
hexGrob <- function(x, y, size = rep(1, length(x)), colour = "grey50", fill = "grey90") {
  stopifnot(length(y) == length(x))

  dx <- resolution(x, FALSE)
  dy <- resolution(y, FALSE) / sqrt(3) / 2 * 1.15

  hexC <- hexbin::hexcoords(dx, dy, n = 1)

  n <- length(x)

  polygonGrob(
    x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(x, each = 6),
    y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(y, each = 6),
    default.units = "native",
    id.lengths = rep(6, n), gp = gpar(col = colour, fill = fill)
  )
}
