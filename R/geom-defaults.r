#' Modify geom/stat aesthetic defaults for future plots
#'
#' @param a_stat,a_geom Name of a_geom/a_stat to modify (like \code{"point"} or
#'   \code{"bin"}), or a a_Geom/a_Stat object (like \code{a_GeomPoint} or
#'   \code{a_StatBin}).
#' @param new Named list of aesthetics.
#' @export
#' @examples
#' update_a_geom_defaults("point", list(colour = "darkblue"))
#' a_plot(mtcars, a_aes(mpg, wt)) + a_geom_point()
#' update_a_geom_defaults("point", list(colour = "black"))
#' @rdname update_defaults
update_a_geom_defaults <- function(a_geom, new) {
  if (is.character(a_geom)) {
    g <- find_subclass("a_Geom", a_geom)
  } else if (inherits(a_geom, "a_Geom")) {
    g <- a_geom
  } else {
    stop('`a_geom` must be a string (like "point") or a a_Geom object (like a_GeomPoint).',
      call. = FALSE)
  }

  old <- g$default_aes
  g$default_aes <- defaults(new, old)
}

#' @rdname update_defaults
#' @export
update_a_stat_defaults <- function(a_stat, new) {
  if (is.character(a_stat)) {
    g <- find_subclass("a_Stat", a_stat)
  } else if (inherits(a_stat, "a_Stat")) {
    g <- a_stat
  } else {
    stop('`stat` must be a string (like "point") or a Stat object (like a_StatBin).',
      call. = FALSE)
  }

  old <- g$default_aes
  g$default_aes <- defaults(new, old)
}
