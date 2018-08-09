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
  path=a_plot()+
    a_theme_bw()+
    a_theme(panel.margin=grid::unit(0, "lines"))+
    a_facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    a_scale_color_manual(values=variable.colors)+
    a_geom_line(a_aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(prostateLasso$path, "weights"))+
    a_geom_line(a_aes(arclength, mse, linetype=set, group=set),
              data=addY(prostateLasso$error, "error"))+
    a_geom_tallrect(a_aes(
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
  res=a_plot()+
    a_geom_hline(a_aes(yintercept=residual),
               data=hline.df,
               color="grey")+
    a_guides(linetype="none")+
    a_geom_point(a_aes(response, residual, 
                   key=observation.i),
               showSelected="arclength",
               shape=21,
               fill=NA,
               color="black",
               data=prostateLasso$residuals)+
    a_geom_segment(a_aes(response, residual,
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
  xpath <- '//g[@class="a_geom3_tallrect_path"]//rect'
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
  r <- getGreyRect(html.click2)
  child.list <- xmlChildren(r)
  expect_identical(names(child.list), "title")
  value.vec <- sapply(child.list, xmlValue)
  expect_identical(paste(value.vec), "arclength 17.4461019561232")
})

viz.time <- viz.no.time
viz.time$time <- list(variable="arclength", ms=5000)

test_that("viz with time option compiles", {
  expect_no_warning({
    info <- animint2HTML(viz.time)
  })
  expect_identical(info$time$sequence, paste(arclength))
})

