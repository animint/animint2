acontext("knitting multiple animint plots in a single Rmd")
knitr::knit_meta() #clear knitr 'metadata'
# Rmd.file <- "~/R/animint/inst/examples/test_knit_print.Rmd" ## Do we need this???
Rmd.file <- system.file("examples", "test_knit_print.Rmd", 
                        package = "animint2")
index.file <- file.path("animint-htmltest", "index.Rmd")

file.copy(Rmd.file, index.file, overwrite=TRUE)
## https://github.com/rstudio/rmarkdown/issues/587#issuecomment-168437646
## @yihui says "Do not use the output_dir argument of render()"
rmarkdown::render(index.file)
remDr$refresh()
Sys.sleep(3)
html <- getHTML()

test_that("knit_print.animint renders five x axis titles", {
  nodes <- getNodeSet(html, "//text[@class='xtitle']")
  value.vec <- sapply(nodes, xmlValue)
  expected.vec <-
    c("first plot with color legend",
      "second plot with color legend",
      "non-interactive plot",
      "position",
      "segments")
  expect_identical(value.vec, expected.vec)
})

test_that("segments and breakpoints are rendered", {
  seg.list <- getNodeSet(html, '//g[@class="geom3_segment_signal"]//line')
  expect_equal(length(seg.list), 6)
  break.list <- getNodeSet(html, '//g[@class="geom4_vline_signal"]//line')
  expect_equal(length(break.list), 5)
})

test_that("svg id property is unique", {
  svg.list <- getNodeSet(html, "//svg")
  attr.mat <- sapply(svg.list, xmlAttrs)
  id.counts <- table(attr.mat["id",])
  expect_true(all(id.counts==1))
})

all.list <- getNodeSet(html, "//*")
id.na.vec <- sapply(all.list, function(e){
  attr.vec.or.null <- xmlAttrs(e)
  if("id" %in% names(attr.vec.or.null)){
    attr.vec.or.null[["id"]]
  }else{
    NA
  }
})
## In HTML, all values are case-insensitive
## http://www.w3schools.com/tags/att_global_id.asp
lower.id.vec <- tolower(id.na.vec)
id.counts <- table(lower.id.vec)
(not.unique <- id.counts[1 < id.counts])
test_that("id property is unique over entire page", {
  expect_equal(length(not.unique), 0)
})

test_that("id must contain at least one character", {
  expect_true(all(0 < nchar(names(id.counts))))
})

test_that("id must not contain any space characters", {
  expect_false(any(grepl(" ", names(id.counts))))
})

## function to extract all circles from an HTML page
get_circles <- function(html=getHTML()) {
  plot.names <- c("plot1top", "plot1bottom")
  count.vec <- c()
  for(i in seq_along(plot.names)){
    xpath <- sprintf("//div[@id='%s']//circle[@class='geom']", plot.names[[i]])
    circle.list <- getNodeSet(html, xpath)
    count.vec[[i]] <- length(circle.list)
  }
  count.vec
}

get_elements <- function(id){
  list(a178=runtime_evaluate_helper(id=id, class_name='show_hide_selector_widgets', list_num=0),
       b934=runtime_evaluate_helper(id=id, class_name='show_hide_selector_widgets', list_num=1),
       show_hide=runtime_evaluate_helper(id=id, class_name='table.legend tr.label_variable', list_num=0),
       widget=runtime_evaluate_helper(id=id, class_name='table.legend tr.label_variable', list_num=1))
}

plot1top <- get_elements("plot1top")
plot1bottom <- get_elements("plot1bottom")

# get_circles() returns a list of circles so comparing with list(expected, expected)

test_that("clicking top legend adds/remove points", {
  expect_equal(get_circles(), list(10, 10))
  clickID("plot1top_q_label_variable_a178")
  expect_equal(get_circles(), list(5, 10))
  clickID("plot1top_q_label_variable_b934")
  expect_equal(get_circles(), list(0, 10))
  clickID("plot1top_q_label_variable_b934")
  expect_equal(get_circles(), list(5, 10))
  clickID("plot1top_q_label_variable_a178")
  expect_equal(get_circles(), list(10, 10))
})

test_that("clicking bottom legend adds/remove points", {
  expect_equal(get_circles(), list(10, 10))
  clickID("plot1bottom_q_label_variable_a178")
  expect_equal(get_circles(), list(10, 5))
  clickID("plot1bottom_q_label_variable_b934")
  expect_equal(get_circles(), list(10, 0))
  clickID("plot1bottom_q_label_variable_b934")
  expect_equal(get_circles(), list(10, 5))
  clickID("plot1bottom_q_label_variable_a178")
  expect_equal(get_circles(), list(10, 10))
})
clickSide <- function(position=NULL){
  id <- paste0("plot1", position)
  runtime_evaluate_helper(id=id, class_name='show_hide_selector_widgets', list_num=0, dispatch_event=TRUE)
  runtime_evaluate_helper(id=id, class_name='selectize-input', list_num=0, dispatch_event=TRUE)
}

sendBackspace <- function() {
  sendKey("Backspace")
  Sys.sleep(0.5)
}

send <- function(alphabet) {
  remDr$Input$insertText(text = alphabet)
  sendKey("Enter")
  Sys.sleep(0.5)
}

clickSide("top")
test_that("top widget adds/remove points", {
  expect_equal(get_circles(), list(10, 10))
  sendBackspace()
  expect_equal(get_circles(), list(5, 10))
  sendBackspace()
  expect_equal(get_circles(), list(0, 10))
  send("a")
  expect_equal(get_circles(), list(5, 10))
  send("b")
  expect_equal(get_circles(), list(10, 10))
})

clickSide("bottom")
test_that("bottom widget adds/remove points", {
  expect_equal(get_circles(), list(10, 10))
  sendBackspace()
  expect_equal(get_circles(), list(10, 5))
  sendBackspace()
  expect_equal(get_circles(), list(10, 0))
  send("a")
  expect_equal(get_circles(), list(10, 5))
  send("b")
  expect_equal(get_circles(), list(10, 10))
})

click_center <- function(id){
  script <- sprintf("document.getElementById('%s').scrollIntoView(true);", id)
  runtime_evaluate(script=script)
  x <- remDr$DOM$getDocument()
  x <- remDr$DOM$querySelector(x$root$nodeId, paste0("#",id))
  x <- remDr$DOM$getBoxModel(x$nodeId)
  m <- matrix(as.numeric(x$model$content), 4, 2,byrow=TRUE, dimnames=list(
    corner=c("left_top", "right_top", "right_bottom", "left_bottom"),
    dim=c("x","y")))
  xy <- as.list((m["left_top",]+m["right_bottom",])/2)
  for(type in c("mousePressed", "mouseReleased")){
    L <- c(xy, button="left", clickCount=1, type=type)
    ## https://github.com/rstudio/chromote/issues/32
    do.call(remDr$Input$dispatchMouseEvent, L)
  }
}

djs.init.list <- driverjs_get(html)
expected.driver.empty <- list(title=list(), description=list())
test_that("knit driver initially shows nothing", {
  expect_identical(djs.init.list, expected.driver.empty)
})

djs.start0.list <- driverjs_start(0)
expected.driver.top <- list(
  title = list(
    text = "geom1_point_q",
    .attrs = c(
      class = "driver-popover-title", 
      style = "display: block;")),
  description = list(
    text = "first plot",
    br=NULL,
    text="Data are shown for the current selection of: label",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;"
    )))
test_that("knit driver start first plot", {
  expect_identical(djs.start0.list, expected.driver.top)
})

click_center("plot1top_q")
djs.start0.top.list <- driverjs_get()
test_that("clicking top plot keeps driver open", {
  expect_identical(djs.start0.top.list, expected.driver.top)
})

click_center("plot1bottom_q")
djs.start0.bottom.list <- driverjs_get()
test_that("clicking bottom plot closes driver", {
  expect_identical(djs.start0.bottom.list, expected.driver.empty)
})

expected.driver.bottom <- list(
  title = list(
    text = "geom1_point_q",
    .attrs = c(
      class = "driver-popover-title", 
      style = "display: block;")),
  description = list(
    text = "second plot", 
    br=NULL,
    text="Data are shown for the current selection of: label",
    .attrs = c(
      class = "driver-popover-description",
      style = "display: block;"
    )))
djs.start1.list <- driverjs_start(1)
test_that("knit driver start second plot", {
  expect_identical(djs.start1.list, expected.driver.bottom)
})

click_center("plot1bottom_q")
djs.start1.bottom.list <- driverjs_get()
test_that("clicking bottom plot keeps driver open", {
  expect_identical(djs.start1.bottom.list, expected.driver.bottom)
})

click_center("plot1top_q")
djs.start1.top.list <- driverjs_get()
test_that("clicking top plot closes driver", {
  expect_identical(djs.start1.top.list, expected.driver.empty)
})

