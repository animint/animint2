acontext("PredictedPeaks data set")

require(httr)
PredictedPeaks.RData <- file.path(tempdir(), "PredictedPeaks.RData")
request <- GET("http://github.com/tdhock/animint-examples/blob/master/data/PredictedPeaks.RData?raw=true")
stop_for_status(request)
writeBin(content(request), PredictedPeaks.RData)
## If we don't load this data set into the global environment, then we
## get Error in eval(expr, envir, enclos) (from helper-functions.R#5)
## : object 'PredictedPeaks' not found
load(PredictedPeaks.RData, .GlobalEnv) 

hover.dots <- subset(PredictedPeaks$chromCounts, nonInputType==type)

viz <- list(
  oneChrom=a_plot()+
    ggtitle("PeakSegJoint detections on selected chromosome")+
    a_theme_bw()+
    a_coord_cartesian(xlim=c(0, 1))+
    a_theme_animint(width=1500, height=100)+
    a_theme(axis.line.x=a_element_blank(), axis.text.x=a_element_blank(), 
          axis.ticks.x=a_element_blank(), axis.title.x=a_element_blank())+
    ## a_geom_text(a_aes(relative.middle, type.fac, a_label=samples.up,
    ##               clickSelects=peak.name,
    ##               showSelected2=chrom,
    ##               showSelected=dotID),
    ##           size=11,
    ##           data=PredictedPeaks$chromCounts)+
    a_geom_text(a_aes(relative.middle, type.fac, a_label=samples.up,
                  href=paste0(
                    "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=",
                    chrom, ":", zoomStart, "-", zoomEnd)),
              showSelected=c("dotID", "chrom"),
              size=11,
              data=PredictedPeaks$chromCounts)+
    a_scale_y_discrete("cell type", drop=FALSE),
  chroms=a_plot()+
    a_theme_bw()+
    a_theme_animint(width=1500, height=330)+
    a_scale_y_discrete("chromosome", drop=FALSE)+ 
    a_scale_x_continuous("position on chromosome (mega bases)")+
    a_geom_text(a_aes(0, chrom, a_label=paste0(peaks, "_")),
              clickSelects="chrom",
              showSelected="dotID",
              hjust=1,
              size=11,
              data=PredictedPeaks$countsByChrom)+
    a_geom_segment(a_aes(chromStart/1e6, chrom,
                     xend=chromEnd/1e6, yend=chrom),
                 clickSelects="chrom",
                 size=9,
                 data=PredictedPeaks$chrom.ranges)+
    a_geom_point(a_aes(chromEnd/1e6, chrom,
                   id=chrom),
               clickSelects="chrom",
               size=5,
               data=PredictedPeaks$chrom.ranges)+
    a_geom_text(a_aes(max(PredictedPeaks$chrom.ranges$chromEnd)/2e6, chrom,
                  a_label=totals),
              showSelected="dotID",
              data=PredictedPeaks$scatter.text),
  scatter=a_plot()+
    a_geom_hline(a_aes(yintercept=N),
               color="grey",
               data=PredictedPeaks$counts.Input)+
    a_scale_x_continuous("number of samples with a peak")+
    a_facet_grid(nonInputType ~ .)+
    a_theme_bw()+
    a_scale_fill_gradient(low="grey", high="red")+
    a_theme_animint(width=1500)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_geom_vline(a_aes(xintercept=N),
               color="grey",
               data=PredictedPeaks$counts.not.Input)+
    a_geom_rect(a_aes(xmin=up-size, xmax=up+size,
                  ymin=Input-size, ymax=Input+size,
                  tooltip=totals,
                  fill=log10(count)),
              clickSelects="dotID",
              showSelected="chrom",
              color="transparent",
              data=PredictedPeaks$bg.rect),
  first=list(dotID="38 neutro samples, 1 Input samples", chrom="chr16"))

info <- animint2HTML(viz)

## Simulate mouseover using javascript?

## myScript <- 'myObj = document.getElementById("chrM");
## myArray = [];
## for(var b in myObj) { 
##   myArray.push(b);
## }
## return myArray;'
## remDr$executeScript(myScript)
## remDr$executeScript('return document.getElementById("chrM").onmouseover();')

## Simulate mouseover using RSelenium?

## e <- remDr$findElement("id", "chrM")
## remDr$mouseMoveToLocation(webElement=e)

## e <- remDr$findElement("id", "chrY")
## remDr$mouseMoveToLocation(webElement=e)

## getStyleValue(getHTML(), '//g[@class="a_geom4_point_chroms"]//circle', "opacity")

## getNodeSet(getHTML(), '//g[@class="a_geom4_point_chroms"]//circle')

test_that("without selectize option, only render chrom widget", {
  widget.vec <- getSelectorWidgets(info$html)
  expect_identical(widget.vec, "chrom")
})

getSorted <- function(){
  text.list <- getNodeSet(getHTML(), '//g[@class="a_geom1_text_oneChrom"]//text')
  value.vec <- sapply(text.list, xmlValue)
  sort(as.numeric(value.vec))
}

test_that("initially 2 text elements rendered", {
  num.vec <- getSorted()
  expect_equal(num.vec, c(1, 38))
})

clickID("chrM")
Sys.sleep(1)

exp.vec <- c(1, 14, 38)

test_that("3 elements rendered (first time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, exp.vec)
})

clickID("chrY")
Sys.sleep(1)

clickID("chrM")
Sys.sleep(1)

test_that("3 elements rendered (second time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, exp.vec)
})

thresh.df <- data.frame(max.input.samples=9, thresh.type="specific")
PredictedPeaks$counts.not.Input$thresh.type <- "max samples"
PredictedPeaks$counts.Input$thresh.type <- "max samples"

viz <- list(
  oneChrom=a_plot()+
    ggtitle("PeakSegJoint detections on selected chromosome")+
    a_theme_bw()+
    a_coord_cartesian(xlim=c(0, 1))+
    a_theme_animint(width=1500, height=100)+
    a_theme(axis.line.x=a_element_blank(), axis.text.x=a_element_blank(), 
          axis.ticks.x=a_element_blank(), axis.title.x=a_element_blank())+
    a_geom_text(a_aes(relative.middle, type.fac, a_label=samples.up),
              showSelected=c("dotID", "chrom"),
              clickSelects="peak.name",
              size=11,
              data=PredictedPeaks$chromCounts)+
    a_scale_y_discrete("cell type", drop=FALSE),
  chroms=a_plot()+
    a_theme_bw()+
    a_theme_animint(width=1500, height=330)+
    a_scale_y_discrete("chromosome", drop=FALSE)+ 
    a_scale_x_continuous("position on chromosome (mega bases)")+
    a_geom_text(a_aes(0, chrom, a_label=paste0(peaks, "_")),
              clickSelects="chrom",
              showSelected="dotID",
              hjust=1,
              size=11,
              data=PredictedPeaks$countsByChrom)+
    a_geom_segment(a_aes(chromStart/1e6, chrom,
                     xend=chromEnd/1e6, yend=chrom),
                 clickSelects="chrom",
                 size=9,
                 data=PredictedPeaks$chrom.ranges)+
    a_geom_point(a_aes(chromEnd/1e6, chrom),
               id="chrom",
               clickSelects="chrom",
               size=5,
               data=PredictedPeaks$chrom.ranges)+
    a_geom_text(a_aes(max(PredictedPeaks$chrom.ranges$chromEnd)/2e6, chrom,
                  a_label=totals),
              showSelected="dotID",
             data=PredictedPeaks$scatter.text),
  scatter=a_plot()+
    a_geom_vline(a_aes(xintercept=N, color=thresh.type),
               data=PredictedPeaks$counts.not.Input)+
    a_scale_color_manual("threshold", values=c(
                                      "max samples"="grey",
                                      specific="grey30"))+
    a_geom_hline(a_aes(yintercept=max.input.samples+0.5, color=thresh.type),
               show.legend=TRUE,
               data=thresh.df)+
    a_geom_hline(a_aes(yintercept=N, color=thresh.type),
               show.legend=TRUE,
               data=PredictedPeaks$counts.Input)+
    a_scale_x_continuous("number of samples with a peak")+
    a_facet_grid(nonInputType ~ .)+
    a_theme_bw()+
    a_scale_fill_gradient(low="grey", high="red")+
    a_theme_animint(width=1500)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_geom_rect(a_aes(xmin=up-size, xmax=up+size,
                  ymin=Input-size, ymax=Input+size,
                  tooltip=totals,
                  fill=log10(count)),
              clickSelects="dotID",
              showSelected="chrom",
              color="transparent",
              data=PredictedPeaks$bg.rect)+
   a_geom_point(a_aes(up, Input),
              showSelected="peak.name",
              data=hover.dots),
  selectize=list(dotID=TRUE, chrom=FALSE),
  first=list(dotID="38 neutro samples, 1 Input samples", chrom="chr16"))

## TODO:href + hoverselects!

info <- animint2HTML(viz)

test_that("selectize option respected", {
  widget.vec <- getSelectorWidgets(info$html)
  expected.widgets <- c("dotID", "thresh.type")
  expect_identical(sort(widget.vec), sort(expected.widgets))
})

test_that("rects rendered in fill legend", {
  rect.list <- getNodeSet(
    info$html, '//tr[@class="log10(count)_variable"]//rect')
  expect_equal(length(rect.list), 5)
})

test_that("no lines rendered in fill legend", {
  line.list <- getNodeSet(
    info$html, '//tr[@class="log10(count)_variable"]//line')
  expect_equal(length(line.list), 0)
})

test_that("lines in color legend", {
  line.list <- getNodeSet(
    info$html, '//tr[@class="thresh_type_variable"]//line')
  expect_equal(length(line.list), 2)
})

specific_hlines <- function(html=getHTML()){
  getNodeSet(html, '//g[@class="a_geom7_hline_scatter"]//line')
}

specific.id <- "plot_scatter_thresh_type_variable_specific"
xpath <- sprintf('//td[@id="%s_a_label"]', specific.id)
specific_opacity <- function(html=getHTML()){
  as.numeric(getStyleValue(html, xpath, "opacity"))
}

test_that("initially rendered hlines", {
  line.list <- specific_hlines(info$html)
  expect_equal(length(line.list), 2)
  computed.opacity <- specific_opacity(info$html)
  expect_equal(computed.opacity, 1)
})

test_that("hlines after clicking specific", {
  html <- clickHTML(id=specific.id)
  line.list <- specific_hlines(html)
  expect_equal(length(line.list), 0)
  computed.opacity <- specific_opacity(html)
  expect_equal(computed.opacity, 0.5)
})

test_that("hlines after clicking specific again", {
  html <- clickHTML(id=specific.id)
  line.list <- specific_hlines(html)
  expect_equal(length(line.list), 2)
  computed.opacity <- specific_opacity(html)
  expect_equal(computed.opacity, 1)
})

## e <- remDr$findElement("class name", "show_hide_selector_widgets")
## e$clickElement()

## remDr$findElements("class name", "selectize-input")

## It takes a long time to render the selectize widget with many
## levels, why?
