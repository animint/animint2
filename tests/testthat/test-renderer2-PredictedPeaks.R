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
SomePeaks <- PredictedPeaks
# Taking minimal subsets
selected_chroms <- c("chr16", "chrM", "chrY")
selected_types <- c("neutro", "mono")
SomePeaks$chromCounts <- subset(
  SomePeaks$chromCounts,
  chrom %in% selected_chroms & 
  type %in% selected_types & 
  samples.up %in% c(1, 14, 38)
)
SomePeaks$countsByChrom <- subset(
  SomePeaks$countsByChrom,
  chrom %in% selected_chroms
)
SomePeaks$chrom.ranges <- subset(
  SomePeaks$chrom.ranges,
  chrom %in% selected_chroms
)
SomePeaks$scatter.text <- subset(
  SomePeaks$scatter.text,
  chrom %in% selected_chroms
)
SomePeaks$bg.rect <- subset(
  SomePeaks$bg.rect,
  chrom %in% selected_chroms & 
  nonInputType %in% selected_types &
  up %in% c(1, 14, 38)
)[1:50,]

hover.dots <- subset(
  SomePeaks$chromCounts,
  nonInputType == type
)

# Ensure we have the specific test cases
test_dotID <- "38 neutro samples, 1 Input samples"
test_chrom <- "chr16"
viz <- list(
  oneChrom=ggplot()+
    ggtitle("PeakSegJoint detections on selected chromosome")+
    theme_bw()+
    coord_cartesian(xlim=c(0, 1))+
    theme_animint(width=800, height=100)+
    theme(
      axis.line.x=element_blank(),
      axis.text.x=element_blank(), 
      axis.ticks.x=element_blank(),
      axis.title.x=element_blank()
    )+
    ## geom_text(aes(relative.middle, type.fac, label=samples.up,
    ##               clickSelects=peak.name,
    ##               showSelected2=chrom,
    ##               showSelected=dotID),
    ##           size=11,
    ##           data=SomePeaks$chromCounts)+
    geom_text(aes(
      relative.middle, type, label=samples.up,
      href=paste0(
        "http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=",
        chrom, ":", zoomStart, "-", zoomEnd
      )),
      showSelected=c("dotID", "chrom"),
      size=11,
      data=SomePeaks$chromCounts
    ) +
    scale_y_discrete("cell type", drop=FALSE),
  chroms=ggplot()+
    theme_bw()+
    theme_animint(width=800, height=200)+
    scale_y_discrete("chromosome", drop=FALSE)+ 
    scale_x_continuous("position on chromosome (mega bases)")+
    geom_text(aes(
      0, chrom, label=paste0(peaks, "_")),
      clickSelects="chrom",
      showSelected="dotID",
      hjust=1,
      size=11,
      data=SomePeaks$countsByChrom
    ) +
    geom_segment(aes(
      chromStart/1e6, chrom,
      xend=chromEnd/1e6, yend=chrom),
      clickSelects="chrom",
      size=5,
      data=SomePeaks$chrom.ranges
    ) +
    geom_point(aes(
      chromEnd/1e6, chrom,
      id=chrom),
      clickSelects="chrom",
      size=3,
      data=SomePeaks$chrom.ranges
    ) +
    geom_text(aes(
      max(SomePeaks$chrom.ranges$chromEnd)/2e6, chrom,
      label=totals),
      showSelected="dotID",
      data=SomePeaks$scatter.text
    ),
  scatter=ggplot()+
    geom_hline(aes(yintercept=N),
      color="grey",
      data=SomePeaks$counts.Input
    ) +
    scale_x_continuous("number of samples with a peak")+
    facet_grid(nonInputType ~ .)+
    theme_bw()+
    scale_fill_gradient(low="grey", high="red")+
    theme_animint(width=800)+
    theme(panel.margin=grid::unit(0, "cm"))+
    geom_vline(aes(xintercept=N),
      color="grey",
      data=SomePeaks$counts.not.Input
    ) +
    geom_rect(aes(
      xmin=up-size, xmax=up+size,
      ymin=Input-size, ymax=Input+size,
      tooltip=totals,
      fill=log10(count)),
      clickSelects="dotID",
      showSelected="chrom",
      color="transparent",
      data=SomePeaks$bg.rect
    ),
  first=list(dotID=test_dotID, chrom=test_chrom)
)

info <- animint2HTML(viz)

Sys.sleep(0.5)
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

## getStyleValue(getHTML(), '//g[@class="geom4_point_chroms"]//circle', "opacity")

## getNodeSet(getHTML(), '//g[@class="geom4_point_chroms"]//circle')

test_that("without selectize option, only render chrom widget", {
  widget.vec <- getSelectorWidgets(info$html)
  expect_identical(widget.vec, "chrom")
})

getSorted <- function(){
  text.list <- getNodeSet(getHTML(), '//g[@class="geom1_text_oneChrom"]//text')
  value.vec <- sapply(text.list, xmlValue)
  sort(as.numeric(value.vec))
}

test_that("initially 1 text element rendered", {
  num.vec <- getSorted()
  expect_equal(num.vec, 38)
})

clickID("chrM")
Sys.sleep(1)

exp.vec <- c(14, 38)

test_that("2 elements rendered (first time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, exp.vec)
})

clickID("chrY")
Sys.sleep(1)

clickID("chrM")
Sys.sleep(1)

test_that("2 elements rendered (second time)", {
  num.vec <- getSorted()
  expect_equal(num.vec, exp.vec)
})

thresh.df <- data.frame(max.input.samples=9,thresh.type="specific")
SomePeaks$counts.not.Input$thresh.type <- "max samples"
SomePeaks$counts.Input$thresh.type <- "max samples"
SomePeaks$bg.rect <- SomePeaks$bg.rect[1:20, ]
hover.dots <- subset(
  SomePeaks$chromCounts,
  type == "neutro" & samples.up %in% c(14, 38)
)

viz <- list(
  oneChrom = ggplot() +
    ggtitle("PeakSegJoint detections on selected chromosome") +
    theme_bw() +
    coord_cartesian(xlim=c(0, 1)) +
    theme_animint(width=800, height=100) +
    theme(
      axis.line.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.title.x = element_blank()
    ) +
    geom_text(aes(relative.middle, type.fac, label=samples.up),
              showSelected=c("dotID", "chrom"),
              clickSelects="peak.name",
              size=10,
              data=SomePeaks$chromCounts) +
    scale_y_discrete("cell type", drop=FALSE),

  chroms = ggplot() +
    theme_bw() +
    theme_animint(width=800, height=250) +
    scale_y_discrete("chromosome", drop=FALSE) + 
    scale_x_continuous("position on chromosome (mega bases)") +
    geom_text(aes(0, chrom, label=paste0(peaks, "_")),
              clickSelects="chrom",
              showSelected="dotID",
              hjust=1,
              size=9,
              data=SomePeaks$countsByChrom) +
    geom_segment(aes(chromStart/1e6, chrom,
                     xend=chromEnd/1e6, yend=chrom),
                 clickSelects="chrom",
                 size=6,
                 data=SomePeaks$chrom.ranges) +
    geom_point(aes(chromEnd/1e6, chrom),
               id="chrom",
               clickSelects="chrom",
               size=3,
               data=SomePeaks$chrom.ranges)+
    geom_text(aes(max(PredictedPeaks$chrom.ranges$chromEnd)/2e6, chrom,
                  label=totals),
              showSelected="dotID",
              data=PredictedPeaks$scatter.text),

  scatter=ggplot() +
    geom_vline(aes(xintercept=N, color=thresh.type),
               data=SomePeaks$counts.not.Input)+
    scale_color_manual("threshold", values=c("max samples"="grey", specific="grey30"))+
    geom_hline(aes(yintercept=max.input.samples+0.5, color=thresh.type),
               show.legend=TRUE,
               data=thresh.df)+
    geom_hline(aes(yintercept=N, color=thresh.type),
               show.legend=TRUE,
               data=SomePeaks$counts.Input)+
    scale_x_continuous("number of samples with a peak") +
    facet_grid(nonInputType ~ .) +
    theme_bw()+
    scale_fill_gradient(low="grey90", high="red")+
    theme_animint(width=600, height=500)+
    geom_rect(aes(xmin=up-size, xmax=up+size,
                  ymin=Input-size, ymax=Input+size,
                  tooltip=totals,
                  fill=log10(count), id=gsub("[^A-Za-z0-9]", "_", paste0("rect_", dotID))),
              clickSelects="dotID",
              showSelected="chrom",
              color="transparent",
              data=SomePeaks$bg.rect)+
    geom_point(aes(up, Input),
               showSelected="peak.name",
               data=hover.dots),
  selectize=list(dotID=TRUE, chrom=FALSE),
  first=list(dotID = test_dotID, chrom = test_chrom)
)

info <- animint2HTML(viz)
test_that("selectize option respected", {
  widget.vec <- getSelectorWidgets(info$html)
  expected.widgets <- c("dotID","thresh.type")
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
  getNodeSet(html, '//g[@class="geom7_hline_scatter"]//line')
}

specific.id <- "plot_scatter_thresh_type_variable_specific"
xpath <- sprintf('//td[@id="%s_label"]', specific.id)
specific_opacity <- function(html=getHTML()){
  as.numeric(getStyleValue(html, xpath, "opacity"))
}

test_that("initially rendered hlines", {
  expect_equal(length(specific_hlines(info$html)), 2)
  expect_equal(specific_opacity(info$html), 1)
})

test_that("hlines after toggling specific twice", {
  clickID(specific.id)
  html1 <- getHTML()
  expect_equal(length(specific_hlines(html1)), 0)
  expect_equal(specific_opacity(html1), 0.5)
  clickID(specific.id)
  html2 <- getHTML()
  expect_equal(length(specific_hlines(html2)), 2)
  expect_equal(specific_opacity(html2), 1)
})

test_that("clicking scatter rect updates chroms summary text", {
  clickID("rect_1_mono_samples__0_Input_samples")
  Sys.sleep(0.2)
  html1 <- getHTML()
  chroms.text1 <- getNodeSet(html1, '//g[@class="geom5_text_chroms"]//text')
  text.value1 <- sapply(chroms.text1, xmlValue)
  expect_true(any(grepl("1 mono samples, 0 Input samples", text.value1)))
})

test_that("clicking scatter rect changes oneChrom text element count", {
  clickID("rect_1_mono_samples__1_Input_samples")
  Sys.sleep(0.2)
  html1 <- getHTML()
  oneChrom.text1 <- getNodeSet(html1, '//g[@class="geom1_text_oneChrom"]//text')
  count1 <- length(oneChrom.text1)
  clickID("rect_1_mono_samples__2_Input_samples")
  Sys.sleep(0.2)
  html2 <- getHTML()
  oneChrom.text2 <- getNodeSet(html2, '//g[@class="geom1_text_oneChrom"]//text')
  count2 <- length(oneChrom.text2)
  expect_true(count1 != count2) # Expect change in label count
})
## e <- remDr$findElement("class name", "show_hide_selector_widgets")
## e$clickElement()

## remDr$findElements("class name", "selectize-input")

## It takes a long time to render the selectize widget with many
## levels, why?
