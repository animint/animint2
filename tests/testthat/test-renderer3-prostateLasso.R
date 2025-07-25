acontext("prostateLasso data set")

data(prostateLasso)

variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999")
hline.df <- data.frame(residual=0)
arclength <- prostateLasso$models$arclength
rect.width <- diff(arclength[1:2])/2
tallrect.all <- expand.grid(
  arclength.click=arclength,
  arclength.show=arclength)
addY <- function(dt, y){
  data.frame(dt, y.var=factor(y, c("error", "weights")))
}

data_tallrect_error <- addY(tallrect.all, "error")
data_tallrect_error$arclength <- "arclength"

viz.no.time <- list(
  title="both .variable .value aesthetics",
  path=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    scale_color_manual(values=variable.colors)+
    geom_line(aes(
      arclength, standardized.coef, color=variable, group=variable),
      help="Regularization path of linear model coefficients, one line for each variable",
      data=addY(prostateLasso$path, "weights"))+
    geom_line(aes(
      arclength, mse, linetype=set, group=set),
      title="Error curves",
      data=addY(prostateLasso$error, "error"))+
    geom_tallrect(aes(
      xmin=arclength.click-rect.width,
      xmax=arclength.click+rect.width,
      id=paste0("arclength", round(arclength.click, 1)*10),
      key=ifelse(
        arclength.click==arclength.show, 1,
        paste(arclength.click, arclength.show))),
      clickSelects=c(arclength="arclength.click"),
      showSelected=c(arclength="arclength.show"),
      alpha=0.5,
      data=data_tallrect_error),
  res=ggplot()+
    geom_hline(aes(yintercept=residual),
               data=hline.df,
               color="grey")+
    guides(linetype="none")+
    geom_point(aes(response, residual, 
                   key=observation.i),
               showSelected="arclength",
               shape=21,
               fill=NA,
               color="black",
               data=prostateLasso$residuals)+
    geom_segment(aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i),
                 showSelected="arclength",
                 data=prostateLasso$residuals),
  first=list(arclength=max(arclength)),
  duration=list(arclength=5000))
info <- animint2HTML(viz.no.time)

clickID("arclength0")
Sys.sleep(1)
html.during <- getHTML()
Sys.sleep(5)
html.after <- getHTML()

getGreyRect <- function(html){
  xpath <- '//g[@class="geom3_tallrect_path"]//rect'
  node.list <- getNodeSet(html, xpath)
  opacity.str <- getStyleValue(html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  grey.i <- which(opacity.num == 0.5)
  node.list[[grey.i]]
}
getGreyX <- function(html){
  grey.rect <- getGreyRect(html)
  attr.vec <- xmlAttrs(grey.rect)
  as.numeric(attr.vec[["x"]])
}

test_that("selected tallrect moves to the left", {
  x.before <- getGreyX(info$html)
  x.during <- getGreyX(html.during)
  expect_lt(x.during, x.before)
  x.after <- getGreyX(html.after)
  expect_lt(x.after, x.during)
})

clickID("arclength174")
html.click2 <- getHTML()

test_that("tallrect displays correct tooltip", {
  # Get tallrect position on the viewport
  tallrect_position <- get_element_bbox("g.geom3_tallrect_path rect#arclength101")
  #hover over the tallrect
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = tallrect_position$left,
    y = tallrect_position$top
  )
  Sys.sleep(0.5)
  # Checking tooltip content
  tooltip_text <- xmlValue(getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]])
  expect_match(tooltip_text, "arclength")
  expect_match(tooltip_text, "\\d+\\.\\d+") # Check for numeric value
  # Clean up - move mouse away
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  Sys.sleep(0.5)
})

viz.time <- viz.no.time
viz.time$time <- list(variable="arclength", ms=5000)

test_that("viz with time option compiles", {
  expect_no_warning({
    info <- animint2HTML(viz.time)
  })
  expect_identical(info$time$sequence, paste(arclength))
})

