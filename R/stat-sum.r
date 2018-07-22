#' @inheritParams layer
#' @inheritParams a_geom_point
#' @section Computed variables:
#' \describe{
#'  \item{n}{number of observations at position}
#'  \item{prop}{percent of points in that panel at that position}
#' }
#' @export
#' @rdname a_geom_count
a_stat_sum <- function(mapping = NULL, data = NULL,
                     a_geom = "point", position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatSum,
    a_geom = a_geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
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
a_StatSum <- a_ggproto("a_StatSum", a_Stat,
  default_aes = aes(size = ..n..),

  required_aes = c("x", "y"),

  compute_panel = function(data, scales) {
    if (is.null(data$weight)) data$weight <- 1

    group_by <- setdiff(intersect(names(data), .all_aesthetics), "weight")

    counts <- plyr::count(data, group_by, wt_var = "weight")
    counts <- plyr::rename(counts, c(freq = "n"), warn_missing = FALSE)
    counts$prop <- stats::ave(counts$n, counts$group, FUN = prop.table)
    counts
  }
)
