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
