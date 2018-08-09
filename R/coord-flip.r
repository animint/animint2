#' Flipped cartesian coordinates.
#'
#' Flipped cartesian coordinates so that horizontal becomes vertical, and
#' vertical, horizontal. This is primarily useful for converting geoms and
#' statistics which display y conditional on x, to x conditional on y.
#'
#' @export
#' @inheritParams a_coord_cartesian
#' @examples
#' # Very useful for creating boxplots, and other interval
#' # geoms in the horizontal instead of vertical position.
#'
#' a_plot(diamonds, a_aes(cut, price)) +
#'   a_geom_boxplot() +
#'   a_coord_flip()
#'
#' h <- a_plot(diamonds, a_aes(carat)) +
#'   a_geom_histogram()
#' h
#' h + a_coord_flip()
#' h + a_coord_flip() + a_scale_x_reverse()
#'
#' # You can also use it to flip line and area plots:
#' df <- data.frame(x = 1:5, y = (1:5) ^ 2)
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_area()
#' last_plot() + a_coord_flip()
a_coord_flip <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  a_ggproto(NULL, a_CoordFlip,
    limits = list(x = xlim, y = ylim),
    expand = expand
  )
}

#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_CoordFlip <- a_ggproto("a_CoordFlip", a_CoordCartesian,

  transform = function(data, scale_details) {
    data <- flip_labels(data)
    a_CoordCartesian$transform(data, scale_details)
  },

  range = function(scale_details) {
    list(x = scale_details$y.range, y = scale_details$x.range)
  },

  train = function(self, scale_details) {
    trained <- a_ggproto_parent(a_CoordCartesian, self)$train(scale_details)
    flip_labels(trained)
  },

  a_labels = function(scale_details) {
    flip_labels(a_CoordCartesian$a_labels(scale_details))
  }
)


flip_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "z", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)

  setNames(x, new_names)
}
