#' @inheritParams a_coord_cartesian
#' @export
#' @rdname a_coord_map
a_coord_quickmap <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  a_ggproto(NULL, a_CoordQuickmap,
    limits = list(x = xlim, y = ylim),
    expand = expand
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_CoordQuickmap <- a_ggproto("a_CoordQuickmap", a_CoordCartesian,

  aspect = function(ranges) {
    # compute coordinates of center point of map
    x.center <- sum(ranges$x.range) / 2
    y.center <- sum(ranges$y.range) / 2

    # compute distance corresponding to 1 degree in either direction
    # from the center
    x.dist <- dist_central_angle(x.center + c(-0.5, 0.5), rep(y.center, 2))
    y.dist <- dist_central_angle(rep(x.center, 2), y.center + c(-0.5, 0.5))
    # NB: this makes the projection correct in the center of the plot and
    #     increasingly less correct towards the edges. For regions of reasonnable
    #     size, this seems to give better results than computing this ratio from
    #     the total lat and lon span.

    # scale the plot with this aspect ratio
    ratio <- y.dist / x.dist

    diff(ranges$y.range) / diff(ranges$x.range) * ratio
  }
)
