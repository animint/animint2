#' ggplot2 a_themes
#'
#' a_themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @details \describe{
#'
#' \item{\code{a_theme_gray}}{
#' The signature ggplot2 a_theme with a grey background and white gridlines,
#' designed to put the data forward yet make comparisons easy.}
#'
#' \item{\code{a_theme_bw}}{
#' The classic dark-on-light ggplot2 a_theme. May work better for presentations
#' displayed with a projector.}
#'
#' \item{\code{a_theme_linedraw}}{
#' A a_theme with only black lines of various widths on white backgrounds,
#' reminiscent of a line drawings. Serves a purpose similar to \code{a_theme_bw}.
#' Note that this a_theme has some very thin lines (<< 1 pt) which some journals
#' may refuse.}
#'
#' \item{\code{a_theme_light}}{
#' A a_theme similar to \code{a_theme_linedraw} but with light grey lines and axes,
#' to direct more attention towards the data.}
#'
#' \item{\code{a_theme_dark}}{
#' The dark cousin of \code{a_theme_light}, with similar line sizes but a dark background. Useful to make thin coloured lines pop out.}
#'
#' \item{\code{a_theme_minimal}}{
#' A minimalistic a_theme with no background annotations.}
#'
#' \item{\code{a_theme_classic}}{
#' A classic-looking a_theme, with x and y axis lines and no gridlines.}
#'
#' \item{\code{a_theme_void}}{
#' A completely empty a_theme.}
#'
#' }
#'
#' @examples
#' p <- a_plot(mtcars) + a_geom_point(a_aes(x = wt, y = mpg,
#'      colour = factor(gear))) + a_facet_wrap(~am)
#'
#' p
#' p + a_theme_gray()
#' p + a_theme_bw()
#' p + a_theme_linedraw()
#' p + a_theme_light()
#' p + a_theme_dark()
#' p + a_theme_minimal()
#' p + a_theme_classic()
#' p + a_theme_void()
#'
#' @name aa_theme
NULL

#' @export
#' @rdname aa_theme
a_theme_grey <- function(base_size = 11, base_family = "") {
  half_line <- base_size / 2

  a_theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               a_element_line(colour = "black", size = 0.5, linetype = 1,
                            lineend = "butt"),
    rect =               a_element_rect(fill = "white", colour = "black",
                            size = 0.5, linetype = 1),
    text =               a_element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),

    axis.line =          a_element_line(),
    axis.line.x =        a_element_blank(),
    axis.line.y =        a_element_blank(),
    axis.text =          a_element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x =        a_element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y =        a_element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks =         a_element_line(colour = "grey20"),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.title.x =       a_element_text(
                           margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)
                         ),
    axis.title.y =       a_element_text(
                           angle = 90,
                           margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
                         ),

    legend.background =  a_element_rect(colour = NA),
    legend.margin =      unit(0.2, "cm"),
    legend.key =         a_element_rect(fill = "grey95", colour = "white"),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        a_element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       a_element_text(hjust = 0),
    legend.title.align = NULL,
    legend.a_position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   a_element_rect(fill = "grey92", colour = NA),
    panel.border =       a_element_blank(),
    panel.grid.major =   a_element_line(colour = "white"),
    panel.grid.minor =   a_element_line(colour = "white", size = 0.25),
    panel.margin =       unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,

    strip.background =   a_element_rect(fill = "grey85", colour = NA),
    strip.text =         a_element_text(colour = "grey10", size = rel(0.8)),
    strip.text.x =       a_element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       a_element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background =    a_element_rect(colour = "white"),
    plot.title =         a_element_text(
                           size = rel(1.2),
                           hjust = 0,
                           margin = margin(b = half_line * 1.2)
                         ),
    plot.subtitle =      a_element_text(
                           size = rel(0.9),
                           hjust = 0,
                           margin = margin(b = half_line * 0.9)
                         ),
    plot.caption =       a_element_text(
                           size = rel(0.9),
                           hjust = 1,
                           margin = margin(b = half_line * 0.9)
                         ),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
#' @export
#' @rdname aa_theme
a_theme_gray <- a_theme_grey

#' @export
#' @rdname aa_theme
a_theme_bw <- function(base_size = 12, base_family = "") {
  # Starts with a_theme_grey and then modify some parts
  a_theme_grey(base_size = base_size, base_family = base_family) %+replace%
    a_theme(
      axis.text         = a_element_text(size = rel(0.8)),
      axis.ticks        = a_element_line(colour = "black"),
      legend.key        = a_element_rect(colour = "grey80"),
      panel.background  = a_element_rect(fill = "white", colour = NA),
      panel.border      = a_element_rect(fill = NA, colour = "grey50"),
      panel.grid.major  = a_element_line(colour = "grey90", size = 0.2),
      panel.grid.minor  = a_element_line(colour = "grey98", size = 0.5),
      strip.background  = a_element_rect(fill = "grey80", colour = "grey50", size = 0.2)
    )
}

#' @export
#' @rdname aa_theme
a_theme_linedraw <- function(base_size = 12, base_family = "") {
  half_line <- base_size / 2
  # Starts with a_theme_grey and then modify some parts
  a_theme_grey(base_size = base_size, base_family = base_family) %+replace%
    a_theme(
      axis.text         = a_element_text(colour = "black", size = rel(0.8)),
      axis.ticks        = a_element_line(colour = "black", size = 0.25),
      legend.key        = a_element_rect(colour = "black", size = 0.25),
      panel.background  = a_element_rect(fill = "white", colour = NA),
      panel.border      = a_element_rect(fill = NA, colour = "black", size = 0.5),
      panel.grid.major  = a_element_line(colour = "black", size = 0.05),
      panel.grid.minor  = a_element_line(colour = "black", size = 0.01),
      strip.background  = a_element_rect(fill = "black", colour = NA),
      strip.text.x      = a_element_text(
                            colour = "white",
                            margin = margin(t = half_line, b = half_line)
                          ),
      strip.text.y      = a_element_text(
                            colour = "white",
                            angle = 90,
                            margin = margin(l = half_line, r = half_line)
                          )
    )
}

#' @export
#' @rdname aa_theme
a_theme_light <- function(base_size = 12, base_family = "") {
  half_line <- base_size / 2
  # Starts with a_theme_grey and then modify some parts
  a_theme_grey(base_size = base_size, base_family = base_family) %+replace%
    a_theme(
      axis.ticks        = a_element_line(colour = "grey70", size = 0.25),
      legend.key        = a_element_rect(fill = "white", colour = "grey50", size = 0.25),
      panel.background  = a_element_rect(fill = "white", colour = NA),
      panel.border      = a_element_rect(fill = NA, colour = "grey70", size = 0.5),
      panel.grid.major  = a_element_line(colour = "grey85", size = 0.25),
      panel.grid.minor  = a_element_line(colour = "grey93", size = 0.125),
      strip.background  = a_element_rect(fill = "grey70", colour = NA),
      strip.text.x      = a_element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = a_element_text(
        colour = "white",
        angle = -90,
        margin = margin(l = half_line, r = half_line)
      )
    )

}

#' @export
#' @rdname aa_theme
a_theme_minimal <- function(base_size = 12, base_family = "") {
  # Starts with a_theme_bw and then modify some parts
  a_theme_bw(base_size = base_size, base_family = base_family) %+replace%
    a_theme(
      legend.background = a_element_blank(),
      legend.key        = a_element_blank(),
      panel.background  = a_element_blank(),
      panel.border      = a_element_blank(),
      strip.background  = a_element_blank(),
      plot.background   = a_element_blank(),
      axis.ticks        = a_element_line(),
      axis.ticks.x      = a_element_blank(),
      axis.ticks.y      = a_element_blank(),
      axis.ticks.length = unit(1, "lines")
    )
}

#' @export
#' @rdname aa_theme
a_theme_classic <- function(base_size = 12, base_family = ""){
  a_theme_bw(base_size = base_size, base_family = base_family) %+replace%
    a_theme(
      panel.border     = a_element_blank(),
      axis.line        = a_element_line(colour = "black"),
      panel.grid.major   = a_element_line(),
      panel.grid.major.x = a_element_blank(),
      panel.grid.major.y = a_element_blank(),
      panel.grid.minor   = a_element_line(),
      panel.grid.minor.x = a_element_blank(),
      panel.grid.minor.y = a_element_blank(),
      strip.background = a_element_rect(colour = "black", size = 0.5),
      legend.key       = a_element_blank()
    )
}

#' @export
#' @rdname aa_theme
a_theme_dark <- function(base_size = 12, base_family = "") {
  half_line <- base_size / 2
  # Starts with a_theme_grey and then modify some parts
  a_theme_grey(base_size = base_size, base_family = base_family) %+replace%
    a_theme(
      axis.ticks        = a_element_line(colour = "grey40", size = 0.25),
      legend.key        = a_element_rect(fill = "grey50", colour = "grey40", size = 0.25),
      panel.background  = a_element_rect(fill = "grey50", colour = NA),
      panel.grid.major  = a_element_line(colour = "grey40", size = 0.25),
      panel.grid.minor  = a_element_line(colour = "grey45", size = 0.125),
      strip.background  = a_element_rect(fill = "grey20", colour = NA),
      strip.text.x      = a_element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = a_element_text(
        colour = "white",
        angle = -90,
        margin = margin(l = half_line, r = half_line)
      )
    )
}

#' @export
#' @rdname aa_theme
a_theme_void <- function(base_size = 12, base_family = "") {
  a_theme(
    # Use only inherited elements and make everything blank
    line =               a_element_blank(),
    rect =               a_element_blank(),
    text =               a_element_text(
                            family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE
                         ),
    plot.margin =        unit(c(0, 0, 0, 0), "lines"),
    axis.text.x =        a_element_blank(),
    axis.text.y =        a_element_blank(),
    axis.title.x =       a_element_blank(),
    axis.title.y =       a_element_blank(),
    legend.text =        a_element_text(size = rel(0.8)),
    legend.title =       a_element_blank(),
    strip.text =         a_element_text(size = rel(0.8)),

    complete = TRUE
  )
}

