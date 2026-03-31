#' Dot plot (removed)
#'
#' @description
#' `geom_dotplot()` has been removed from animint2 due to fundamental issues
#' with interactive rendering. Please use `geom_point()` instead.
#'
#' @export
#' @keywords internal
geom_dotplot <- function(...) {
  stop("geom_dotplot() has been removed from animint2. Use geom_point() instead. See issue #289 for details: https://github.com/animint/animint2/issues/289")
}
