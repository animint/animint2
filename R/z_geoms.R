#' ggplot2 a_geom with xmin and xmax aesthetics that covers the entire y range, useful for clickSelects background elements.
#' @param mapping aesthetic mapping
#' @param data data set
#' @param a_stat statistic mapping, defaults to identity
#' @param a_position position mapping, defaults to identity
#' @param ... other arguments
#' @param na.rm ...
#' @param show.legend ...
#' @param inherit.a_aes ...
#' @return ggplot2 layer
#' @export
#' @example inst/examples/breakpoints.R
a_geom_tallrect <- function(mapping = NULL, data = NULL,
                          a_stat = "identity", a_position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    a_geom = a_GeomTallRect,
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
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
a_GeomTallRect <- a_ggproto("a_GeomTallRect", a_Geom,
                                 default_aes = a_aes(colour = "grey35",
                                                   fill = "grey35", 
                                                   size = 0.5, 
                                                   linetype = 1,
                                                   alpha = 0.5),
                                 
                                 required_aes = c("xmin", "xmax"),
                                 
                                 draw_panel = function(self, data, 
                                                       panel_scales, a_coord) {
                                   coords <- a_coord$transform(data, panel_scales)
                                   ymax <- grid::unit(1, "npc")
                                   ymin <- grid::unit(0, "npc")
                                   grid::rectGrob(
                                     coords$xmin, ymax,
                                     width = coords$xmax - coords$xmin,
                                     height = ymax - ymin,
                                     default.units = "native",
                                     just = c("left", "top"),
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       fill = scales::alpha(coords$fill, 
                                                            coords$alpha), 
                                       lwd = coords$size * .pt,
                                       lty = coords$linetype,
                                       lineend = "butt"
                                     )
                                   )
                                 },
                                 
                                 draw_key = a_draw_key_rect
)


#' ggplot2 geom with ymin and ymax aesthetics that covers the entire x range, useful for clickSelects background elements.
#' @param mapping aesthetic mapping
#' @param data data set
#' @param a_stat statistic mapping, defaults to identity
#' @param a_position position mapping, defaults to identity
#' @param ... other arguments
#' @param na.rm ...
#' @param show.legend ...
#' @param inherit.a_aes ...
#' @return ggplot2 layer
#' @export
#' @examples
#'  \dontrun{ 
#'    source(system.file("examples/WorldBank.R", package = "animint"))
#'  }
a_geom_widerect <- function(mapping = NULL, data = NULL,
                          a_stat = "identity", a_position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.a_aes = TRUE) {
  a_layer(
    a_geom = a_GeomWideRect,
    data = data,
    mapping = mapping,
    a_stat = a_stat,
    a_position = a_position,
    show.legend = show.legend,
    inherit.a_aes = inherit.a_aes,
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
a_GeomWideRect <- a_ggproto("a_GeomWideRect", a_Geom,
                                 default_aes = a_aes(colour = "grey35", 
                                                   fill = "grey35", 
                                                   size = 0.5, 
                                                   linetype = 1,
                                                   alpha = 0.5),
                                 
                                 required_aes = c("ymin", "ymax"),
                                 
                                 draw_panel = function(self, data, 
                                                       panel_scales, a_coord) {
                                   coords <- a_coord$transform(data, panel_scales)
                                   xmax <- grid::unit(1, "npc")
                                   xmin <- grid::unit(0, "npc")
                                   grid::rectGrob(
                                     xmin, coords$ymin,
                                     width = xmax - xmin,
                                     height = coords$ymax - coords$ymin,
                                     default.units = "native",
                                     just = c("left", "bottom"),
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       fill = scales::alpha(coords$fill, 
                                                            coords$alpha), 
                                       lwd = coords$size * .pt,
                                       lty = coords$linetype,
                                       lineend = "butt"
                                     )
                                   )
                                 },
                                 
                                 draw_key = a_draw_key_rect
)

#' Make a clickSelects a_geom_tallrect that completely tiles the x
#' range. This makes it easy to construct tallrects for the common
#' case of selecting a particular x value.
#' @param data data.frame to analyze for unique x.name values.
#' @param x.name variable to be used for x, clickSelects.
#' @param even Logical parameter, should tallrects be of even width?
#' @param alpha transparency of a selected tallrect, default 1/2.
#' @param ... passed to a_geom_tallrect.
#' @return a a_geom_tallrect layer.
#' @author Toby Dylan Hocking
#' @export
make_tallrect <- function(data, x.name, even=FALSE, alpha=1/2, ...){
  make_tallrect_or_widerect(
    "x", a_geom_tallrect, data, x.name, even, alpha, ...)
}

#' Make a clickSelects a_geom_widerect that completely tiles the y
#' range. This makes it easy to construct widerects for the common
#' case of selecting a particular y value.
#' @param data data.frame to analyze for unique y.name values.
#' @param y.name variable to be used for y, clickSelects.
#' @param even Logical parameter, should widerects be of even width?
#' @param alpha transparency of a selected widerect, default 1/2.
#' @param ... passed to a_geom_widerect.
#' @return a a_geom_widerect layer.
#' @author Toby Dylan Hocking
#' @export
make_widerect <- function(data, y.name, even=FALSE, alpha=0.5, ...){
  make_tallrect_or_widerect(
    "y", a_geom_widerect, data, y.name, even, alpha, ...)
}

#' Make a clickSelects a_geom_widerect or a_geom_tallrect that completely
#' tiles the x or y range. This function is used internally by
#' make_tallrect or make_widerect, which are more user-friendly.
#' @param a_aes.prefix "x" or "y"
#' @param a_geom_xrect a_geom_tallrect or a_geom_widerect
#' @param data data.frame to analyze for unique var.name values.
#' @param var.name variable to be used for clickSelects
#' @param even Logical parameter, should xrects be of even width?
#' @param alpha transparency of a selected xrect, default 1/2.
#' @param ... passed to a_geom_xrect
#' @param data.fun called on data passed to a_geom_xrect(a_aes(..),
#'   data.fun(df)) this is useful in facetted plots, for adding
#'   columns to the data.frame, if you want that a_geom in only one
#'   panel.
#' @return a a_geom_xrect layer
#' @author Toby Dylan Hocking
#' @export
make_tallrect_or_widerect <- function(a_aes.prefix, a_geom_xrect, data, var.name, even=FALSE, alpha=0.5, ..., data.fun=identity){
  stopifnot(is.character(a_aes.prefix))
  stopifnot(length(a_aes.prefix)==1)
  stopifnot(a_aes.prefix %in% c("x", "y"))
  stopifnot(is.function(a_geom_xrect))
  data <- as.data.frame(data)
  stopifnot(is.character(var.name))
  stopifnot(length(var.name)==1)
  x <- data[, var.name]
  stopifnot(is.numeric(x))
  stopifnot(is.logical(even))
  stopifnot(length(even)==1)
  stopifnot(is.numeric(alpha))
  stopifnot(length(alpha)==1)
  stopifnot(is.function(data.fun))
  vals <- sort(unique(x))
  Delta <- if(even) rep(resolution(vals), length(vals)-1)/2 else diff(vals)/2
  breaks <- c(vals[1] - Delta[1],
              vals[-1] - Delta,
              vals[length(vals)]+Delta[length(Delta)])
  stopifnot(length(breaks) == length(vals)+1)
  df <- data.frame(vals,
                   min=breaks[-length(breaks)],
                   max=breaks[-1])
  a_geom.df <- expand.grid(click.i=1:nrow(df), show.i=1:nrow(df))
  a_geom.df$click.val <- df[a_geom.df$click.i, "vals"]
  a_geom.df$show.val <- df[a_geom.df$show.i, "vals"]
  a_geom.df$var <- var.name
  a_geom.df$key <- with(a_geom.df, ifelse(
    click.val==show.val, 1,
    paste(click.val, show.val)))
  a_aes.string.args <- list()
  ss_params <- c(var="show.val")
  cs_params <- c(var="click.val")
  a_aes.string.args[["key"]] <- "key"
  for(suffix in c("min", "max")){
    a_aes.str <- paste0(a_aes.prefix, suffix)
    a_geom.df[[suffix]] <- df[a_geom.df$click.i, suffix]
    a_aes.string.args[[a_aes.str]] <- suffix
  }
  a <- do.call(a_aes_string, a_aes.string.args)
  a_geom_xrect(a, data.fun(a_geom.df),
             showSelected = ss_params, clickSelects = cs_params,
             alpha=alpha, ...)
}

#' Convenience function for an interactive bar that might otherwise be
#' created using a_stat_summary(a_geom="bar").
#' @param data data.frame to analyze for unique x.name values.
#' @param x.name variable to be used for x, clickSelects.
#' @param alpha transparency of selected bar, default 1.
#' @return a a_geom_bar layer.
#' @author Toby Dylan Hocking
#' @export
make_bar <- function(data, x.name, alpha=1){
  data <- as.data.frame(data)
  stopifnot(is.character(x.name))
  stopifnot(length(x.name)==1)
  x <- data[,x.name]
  stopifnot(is.numeric(x))
  a_stat_summary(a_aes_string(x=x.name, y=x.name), clickSelects=x.name,
               data=data, alpha=alpha, fun.y=length, a_geom="bar")
}

#' Convenvience function for a showSelected plot label.
#' @param data data.frame of relevant data
#' @param x x coordinate of label position
#' @param y y coordinate of label position
#' @param a_label.var variable matching showSelected, used to obtain label value
#' @param format String format for label. Use \%d, \%f, etc. to insert relevant label.var value.
#' @return a a_geom_text layer.
#' @author Toby Dylan Hocking
#' @export
make_text <- function(data, x, y, a_label.var, format=NULL){
  data <- as.data.frame(data)
  stopifnot(length(x)==1)
  stopifnot(length(y)==1)
  ## TODO: position based on the data?
  ## if(is.character(x) && x %in% names(data)){
  ##   x <- data[,x]
  ##   x <- (min(x)+max(x))/2
  ## }
  ## if(is.character(y) && y %in% names(data)){
  ##   y <- max(data[,y])
  ## }
  data <- unique(data[,a_label.var,drop=FALSE])
  data$a_label <- data[,a_label.var]
  data$x <- x
  data$y <- y
  if(is.null(format)){
    data$a_label <- as.character(data$a_label)
    format <- paste(a_label.var,"= %s")
  }
  if(is.character(format)){
    fstring <- format
    format <- function(val){
      sprintf(fstring, val)
    }
  }
  stopifnot(is.function(format))
  data$a_label <- format(data$a_label)
  a <- a_aes_string(x="x",y="y",a_label="a_label")
  a_geom_text(a, showSelected=a_label.var, data)
}
