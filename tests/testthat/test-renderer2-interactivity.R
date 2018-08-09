acontext("interactivity")

## Example: 2 plots, 2 selectors, but only interacting with 1 plot.
data(breakpoints, package = "animint2")
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error, samples==samples[1])
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")
breakpointError <- list(
  signal=a_plot()+
    a_geom_point(a_aes(position, signal),
               showSelected="samples",
               data=breakpoints$signals)+
    a_geom_line(a_aes(position, signal), colour=signal.colors[["latent"]],
              data=breakpoints$imprecision)+
    a_geom_segment(a_aes(first.base, mean, xend=last.base, yend=mean),
                 showSelected=c("segments", "samples"),
                 colour=signal.colors[["estimate"]],
                 data=breakpoints$segments)+
    a_geom_vline(a_aes(xintercept=base),
               showSelected=c("segments", "samples"),
               colour=signal.colors[["estimate"]],
               linetype="dashed",
               data=breakpoints$breaks),
  points=a_plot()+
    a_geom_point(a_aes(samples, error,
                   id=paste0("samples", samples)),
               showSelected="segments",
               clickSelects="samples",
               data=only.error, stat="identity"),
  error=a_plot()+
    a_geom_vline(a_aes(xintercept=segments, 
                   id=paste0("segments", segments)),
               clickSelects="segments",
               data=only.segments, lwd=17, alpha=1/2)+
    a_geom_line(a_aes(segments, error, group=samples),
              clickSelects="samples",
              data=only.error, lwd=4),
  first=list(samples=150, segments=4),
  title="breakpointError (select one model size)")

info <- animint2HTML(breakpointError)
##remDr$screenshot(file="~/R/animint/phantom-bug-screenshot.png")

dasharrayPattern <-
  paste0("stroke-dasharray:",
         "(?<value>.*?)",
         ";")

get.dasharray <- function(node.set){
  expect_true(length(node.set) > 0)
  style.list <- list()
  for(node.i in seq_along(node.set)){
    node <- node.set[[node.i]]
    a.vec <- xmlAttrs(node)
    style.list[[node.i]] <- a.vec[["style"]]
  }
  style.vec <- do.call(c, style.list)
  str_match_perl(style.vec, dasharrayPattern)
}

dashed.xpaths <-
  c('//g[@class="a_geom4_vline_signal"]//g[@class="PANEL1"]//line')
solid.xpaths <- 
  c('//g[@class="a_geom2_line_signal"]//g[@class="PANEL1"]//path',
    '//g[@class="a_geom3_segment_signal"]//g[@class="PANEL1"]//line',
    '//g[@class="a_geom6_vline_error"]//g[@class="PANEL1"]//line',
    '//g[@class="a_geom7_line_error"]//g[@class="PANEL1"]//path')

test_that("stroke-dasharray: 8,8 for dashed", {
  for(xpath in dashed.xpaths){
    node.set <- getNodeSet(info$html, xpath)
    dash.mat <- get.dasharray(node.set)
    expect_match(dash.mat[, "value"], "8(px)?, *8(px)?")
  }
})

test_that("stroke-dasharray default solid (no stroke-dasharray)", {
  for(xpath in solid.xpaths){
    node.set <- getNodeSet(info$html, xpath)
    dash.mat <- get.dasharray(node.set)
    expect_true(all(is.na(dash.mat[, 1])))
  }
})

test_that("default is single selection", {
  selector.types <- lapply(info$selectors, "[[", "type")
  expect_match(selector.types$samples, "single")
  expect_match(selector.types$segments, "single")
})

test_that("a_aes(id) geoms have ids", {
  nodes <- getNodeSet(info$html, '//*[@id][@class="a_geom"]')
  expect_equal(length(nodes), 24)
})

test_that("4 <circle> elements in second plot", {
  nodes <- getNodeSet(info$html, '//g[@class="a_geom5_point_points"]//circle')
  expect_equal(length(nodes), 4)
})

test_that("default is 150 <circle> elements", {
  nodes <- getNodeSet(info$html, '//g[@class="a_geom1_point_signal"]//circle')
  expect_equal(length(nodes), 150)
})

test_that("default is 4 <line> segments", {
  nodes <- getNodeSet(info$html, '//g[@class="a_geom3_segment_signal"]//line')
  expect_equal(length(nodes), 4)
})

test_that("clickSelects 300 makes 300 <circle> elements", {
  html <- clickHTML(id=paste0("samples", 300))
  nodes <- getNodeSet(html, '//g[@class="a_geom1_point_signal"]//circle')
  expect_equal(length(nodes), 300)
})

test_that("clickSelects 1 changes to 1 <line> element", {
  html <- clickHTML(id=paste0("segments", 1))
  nodes <- getNodeSet(html, '//g[@class="a_geom3_segment_signal"]//line')
  expect_equal(length(nodes), 1)
})

breakpointError$selector.types <-
  list(segments="multiple",
       samples="single")
breakpointError$title <-
  "breakpointError (select several model sizes)"
info <- animint2HTML(breakpointError)

test_that("selector.types are converted to JSON", {
  selector.types <- lapply(info$selectors, "[[", "type")
  expect_match(selector.types$samples, "single")
  expect_match(selector.types$segments, "multiple")
})

test_that("default is 150 and 4 <circle> elements", {
  nodes <- getNodeSet(info$html, '//g[@class="a_geom1_point_signal"]//circle')
  expect_equal(length(nodes), 150)
  nodes <- getNodeSet(info$html, '//g[@class="a_geom5_point_points"]//circle')
  expect_equal(length(nodes), 4)
})

test_that("clickSelects 300 makes 300 <circle> elements", {
  html <- clickHTML(id=paste0("samples", 300))
  nodes <- getNodeSet(html, '//g[@class="a_geom1_point_signal"]//circle')
  expect_equal(length(nodes), 300)
})

test_that("clickSelects 1 adds 1 <line> and 4 <circle>", {
  html <- clickHTML(id=paste0("segments", 1))
  nodes <- getNodeSet(html, '//g[@class="a_geom3_segment_signal"]//line')
  expect_equal(length(nodes), 5)
  nodes <- getNodeSet(html, '//g[@class="a_geom5_point_points"]//circle')
  expect_equal(length(nodes), 8)
})

test_that("clickSelects 4 removes 4 <line> elements and 4 <circle>", {
  html <- clickHTML(id=paste0("segments", 4))
  nodes <- getNodeSet(html, '//g[@class="a_geom3_segment_signal"]//line')
  expect_equal(length(nodes), 1)
  nodes <- getNodeSet(html, '//g[@class="a_geom5_point_points"]//circle')
  expect_equal(length(nodes), 4)
})

test_that("clickSelects 1 removes all <line> elements and all <circle>", {
  html <- clickHTML(id=paste0("segments", 1))
  nodes <- getNodeSet(html, '//g[@class="a_geom3_segment_signal"]//line')
  expect_equal(length(nodes), 0)
  nodes <- getNodeSet(html, '//g[@class="a_geom5_point_points"]//circle')
  expect_equal(length(nodes), 0)
})

## Tornado multiple selection.
data(UStornadoes, package = "animint2")
stateOrder <- data.frame(state = unique(UStornadoes$state)[order(unique(UStornadoes$TornadoesSqMile), decreasing=T)], rank = 1:49) # order states by tornadoes per square mile
UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]
library(plyr)
UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))
seg.color <- "#55B1F7"
tornado.lines <-
  list(map=a_plot()+
       make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in %d")+
       a_geom_polygon(a_aes(x=long, y=lat, group=group,
                        id=state),
                    clickSelects="state",
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour=seg.color, data=UStornadoes)+
       a_scale_fill_manual(values=c(end=seg.color))+
       a_theme_animint(width=750, height=500)+
       a_geom_point(a_aes(endLong, endLat, fill=place),
                  colour=seg.color, showSelected="year",
                  data=data.frame(UStornadoes,place="end")),
       ts=a_plot()+
       a_geom_text(a_aes(year, count, a_label=state),
                 hjust=0, showSelected="state",
                 data=subset(UStornadoCounts, year==max(year)))+
       make_tallrect(UStornadoCounts, "year")+
       a_geom_line(a_aes(year, count,
                     group=state),
                 showSelected="state",
                data=UStornadoCounts),
       selector.types=list(state="multiple"),
       first=list(state=c("CA", "NY"), year=1950))

test_that("1950 <circle> and <line> elements", {
  ## A warning should be issued when there is showSelected=place and
  ## there is only 1 value for place, but not when there is a legend
  ## (as in this data viz).
  expect_no_warning({
    info <- animint2HTML(tornado.lines)
  })
  t1950 <- subset(UStornadoes, year==1950)
  nodes <- getNodeSet(info$html, '//g[@class="a_geom3_segment_map"]//line')
  expect_equal(length(nodes), nrow(t1950))
  nodes <- getNodeSet(info$html, '//g[@class="a_geom4_point_map"]//circle')
  expect_equal(length(nodes), nrow(t1950))
  nodes <- getNodeSet(info$html, '//g[@class="a_geom7_line_ts"]//path')
  expect_equal(length(nodes), 2)
  nodes <- getNodeSet(info$html, '//g[@class="a_geom5_text_ts"]//text')
  expect_equal(length(nodes), 2)
})

test_that("clickSelects CO adds 1 <path> and 1 <text>", {
  html <- clickHTML(id="CO")
  nodes <- getNodeSet(html, '//g[@class="a_geom7_line_ts"]//path')
  expect_equal(length(nodes), 3)
  nodes <- getNodeSet(html, '//g[@class="a_geom5_text_ts"]//text')
  expect_equal(length(nodes), 3)
})

test_that("clickSelects CA removes 1 <path> and 1 <text>", {
  html <- clickHTML(id="CA")
  nodes <- getNodeSet(html, '//g[@class="a_geom7_line_ts"]//path')
  expect_equal(length(nodes), 2)
  nodes <- getNodeSet(html, '//g[@class="a_geom5_text_ts"]//text')
  expect_equal(length(nodes), 2)
})
