#' Marginal rug plots.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "rug")}
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param sides A string that controls which sides of the plot the rugs appear on.
#'   It can be set to a string containing any of \code{"trbl"}, for top, right,
#'   bottom, and left.
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(wt, mpg))
#' p + a_geom_point()
#' p + a_geom_point() + a_geom_rug()
#' p + a_geom_point() + a_geom_rug(sides="b")    # Rug on bottom only
#' p + a_geom_point() + a_geom_rug(sides="trbl") # All four sides
#' p + a_geom_point() + a_geom_rug(a_position='jitter')
a_geom_rug <- function(mapping = NULL, data = NULL,
                     a_stat = "identity", a_position = "identity",
                     ...,
                     sides = "bl",
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.a_aes = TRUE) {
  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomRug,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      sides = sides,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomRug <- a_ggproto("a_GeomRug", a_Geom,
  draw_panel = function(data, panel_scales, a_coord, sides = "bl") {
    rugs <- list()
    data <- a_coord$transform(data, panel_scales)

    gp <- gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
    if (!is.null(data$x)) {
      if (grepl("b", sides)) {
        rugs$x_b <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(0, "npc"), y1 = unit(0.03, "npc"),
          gp = gp
        )
      }

      if (grepl("t", sides)) {
        rugs$x_t <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(1, "npc"), y1 = unit(0.97, "npc"),
          gp = gp
        )
      }
    }

    if (!is.null(data$y)) {
      if (grepl("l", sides)) {
        rugs$y_l <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(0, "npc"), x1 = unit(0.03, "npc"),
          gp = gp
        )
      }

      if (grepl("r", sides)) {
        rugs$y_r <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(1, "npc"), x1 = unit(0.97, "npc"),
          gp = gp
        )
      }
    }

    gTree(children = do.call("gList", rugs))
  },

  default_aes = a_aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_key = a_draw_key_path
)
