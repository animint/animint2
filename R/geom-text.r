#' Textual annotations.
#'
#' \code{a_geom_text} adds text directly to the plot. \code{a_geom_label} draws
#' a rectangle underneath the text, making it easier to read.
#'
#' Note the the "width" and "height" of a text element are 0, so stacking
#' and dodging text will not work by default, and axis limits are not
#' automatically expanded to include all text. Obviously, labels do have
#' height and width, but they are physical units, not data units. The amount of
#' space they occupy on that plot is not constant in data units: when you
#' resize a plot, labels stay the same size, but the size of the axes changes.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{animint2:::rd_aesthetics("a_geom", "text")}
#'
#' @section \code{a_geom_label}:
#' Currently \code{a_geom_label} does not support the \code{rot} parameter and
#' is considerably slower than \code{a_geom_text}. The \code{fill} aesthetic
#' controls the background colour of the label.
#'
#' @section Alignment:
#' You can modify text alignment with the \code{vjust} and \code{hjust}
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character ("left", "middle", "right", "bottom", "center",
#' "top"). There are two special alignments: "inward" and "outward".
#' Inward always aligns text towards the center, and outward aligns
#' it away from the center
#'
#' @inheritParams a_layer
#' @inheritParams a_geom_point
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same a_layer will not be plotted. A quick and dirty way
#' @export
#' @examples
#' p <- a_plot(mtcars, a_aes(wt, mpg, a_label = rownames(mtcars)))
#'
#' p + a_geom_text()
#' # Avoid overlaps
#' p + a_geom_text(check_overlap = TRUE)
#' # Labels with background
#' p + a_geom_label()
#' # Change size of the label
#' p + a_geom_text(size = 10)
#'
#' # Set a_aesthetics to fixed value
#' p + a_geom_point() + a_geom_text(hjust = 0, nudge_x = 0.05)
#' p + a_geom_point() + a_geom_text(vjust = 0, nudge_y = 0.5)
#' p + a_geom_point() + a_geom_text(angle = 45)
#' \dontrun{
#' # Doesn't work on all systems
#' p + a_geom_text(family = "Times New Roman")
#' }
#'
#' # Add a_aesthetic mappings
#' p + a_geom_text(a_aes(colour = factor(cyl)))
#' p + a_geom_text(a_aes(colour = factor(cyl))) +
#'   a_scale_colour_discrete(l = 40)
#' p + a_geom_label(a_aes(fill = factor(cyl)), colour = "white", fontface = "bold")
#'
#' p + a_geom_text(a_aes(size = wt))
#' # a_scale height of text, rather than sqrt(height)
#' p + a_geom_text(a_aes(size = wt)) + a_scale_radius(range = c(3,6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # a_geom_text uses strings, not expressions.
#' p + a_geom_text(a_aes(a_label = paste(wt, "^(", cyl, ")", sep = "")),
#'   parse = TRUE)
#'
#' # Add a text annotation
#' p + a_geom_text() + a_annotate("text",
#' a_label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")
#'
#' \donttest{
#' # Aligning labels and bars --------------------------------------------------
#' df <- data.frame(
#'   x = factor(c(1, 1, 2, 2)),
#'   y = c(1, 3, 2, 1),
#'   grp = c("a", "b", "a", "b")
#' )
#'
#' # ggplot2 doesn't know you want to give the labels the same virtual width
#' # as the bars:
#' a_plot(data = df, a_aes(x, y, fill = grp, a_label = y)) +
#'   a_geom_bar(a_stat = "identity", a_position = "dodge") +
#'   a_geom_text(a_position = "dodge")
#' # So tell it:
#' a_plot(data = df, a_aes(x, y, fill = grp, a_label = y)) +
#'   a_geom_bar(a_stat = "identity", a_position = "dodge") +
#'   a_geom_text(a_position = a_position_dodge(0.9))
#' # Use you can't nudge and dodge text, so instead adjust the y postion
#' a_plot(data = df, a_aes(x, y, fill = grp, a_label = y)) +
#'   a_geom_bar(a_stat = "identity", a_position = "dodge") +
#'   a_geom_text(a_aes(y = y + 0.05), a_position = a_position_dodge(0.9), vjust = 0)
#'
#' # To place text in the middle of each bar in a stacked barplot, you
#' # need to do the computation yourself
#' df <- transform(df, mid_y = ave(df$y, df$x, FUN = function(val) cumsum(val) - (0.5 * val)))
#'
#' a_plot(data = df, a_aes(x, y, fill = grp, a_label = y)) +
#'  a_geom_bar(a_stat = "identity") +
#'  a_geom_text(a_aes(y = mid_y))
#'
#' # Justification -------------------------------------------------------------
#' df <- data.frame(
#'   x = c(1, 1, 2, 2, 1.5),
#'   y = c(1, 2, 1, 2, 1.5),
#'   text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
#' )
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_text(a_aes(a_label = text))
#' a_plot(df, a_aes(x, y)) +
#'   a_geom_text(a_aes(a_label = text), vjust = "inward", hjust = "inward")
#' }
a_geom_text <- function(mapping = NULL, data = NULL,
                      a_stat = "identity", a_position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.a_aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(a_position)) {
      stop("Specify either `a_position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    a_position <- a_position_nudge(nudge_x, nudge_y)
  }

  a_layer(
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_geom = a_GeomText,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname animint2-ggproto
#' @format NULL
#' @usage NULL
#' @export
a_GeomText <- a_ggproto("a_GeomText", a_Geom,
  required_aes = c("x", "y", "a_label"),

  default_aes = a_aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_scales, a_coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    lab <- data$a_label
    if (parse) {
      lab <- parse(text = as.character(lab))
    }

    data <- a_coord$transform(data, panel_scales)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = a_draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
