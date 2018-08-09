#' \code{a_stat_count} counts the number of cases at each x position. If you want
#' to bin the data in ranges, you should use \code{\link{a_stat_bin}} instead.
#'
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
#' @seealso \code{\link{a_stat_bin}}, which bins data in ranges and counts the
#'   cases in each range. It differs from \code{a_stat_count}, which counts the
#'   number of cases at each x position (without binning into ranges).
#'   \code{\link{a_stat_bin}} requires continuous x data, whereas
#'   \code{a_stat_count} can be used for both discrete and continuous x data.
#'
#' @export
#' @rdname a_geom_bar
a_stat_count <- function(mapping = NULL, data = NULL,
                       a_geom = "bar", a_position = "stack",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_StatCount,
    a_geom = a_geom,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      na.rm = na.rm,
      width = width,
      ...
    )
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
a_StatCount <- a_ggproto("a_StatCount", a_Stat,
  required_aes = "x",
  default_aes = a_aes(y = ..count..),

  setup_params = function(data, params) {
    if (!is.null(data$y)){
      stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
    }
    if (!is.null(params$y)) {
      stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
    }
    params
  },

  compute_group = function(self, data, scales, width = NULL) {
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))
    width <- width %||% (resolution(x) * 0.9)

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    data.frame(
      count = count,
      prop = count / sum(abs(count)),
      x = sort(unique(x)),
      width = width
    )
  }
)
