acontext("variable value")

problems <-
  data.frame(problemStart=c(100, 200, 100, 150, 200, 250),
             problemEnd=c(200, 300, 150, 200, 250, 300),
             problem.i=c(1, 2, 1, 2, 3, 4),
             bases.per.problem=c(100, 100, 50, 50, 50, 50))
problems$problem.name <- with(problems, {
  sprintf("size.%d.problem.%d", bases.per.problem, problem.i)
})

sizes <- data.frame(bases.per.problem=c(50, 100),
                    problems=c(2, 4))

problems$peakStart <- problems$problemStart + 10
problems$peakEnd <- problems$problemEnd - 10

samples <-
  rbind(data.frame(problems, sample.id="sample1", peaks=1),
        data.frame(problems, sample.id="sample1", peaks=2),        
        data.frame(problems, sample.id="sample2", peaks=2))

peaks <-
  expand.grid(peaks=0:2, 
              problem.name=problems$problem.name)
peaks$error.type <-
  c("false positive", "false negative", "correct")
rownames(problems) <- problems$problem.name
peaks$bases.per.problem <-
  problems[paste(peaks$problem.name), "bases.per.problem"]

peak.problems <-
  rbind(data.frame(problems, peaks=1),
        data.frame(problems, peaks=2))

one.error <-
  data.frame(bases.per.problem=1:10,
             errors=rnorm(10),
             chunks="one")
two.error <-
  data.frame(bases.per.problem=1:10,
             errors=rnorm(10),
             chunks="two")

showSelected.vec <- c(problem.name="peaks", "bases.per.problem")
clickSelects.vec <- c(problem.name="peaks")

viz <-
  list(errorLines=ggplot()+
         scale_color_manual(values=c(one="red", two="black"))+
         scale_size_manual(values=c(one=1, two=2))+
         geom_line(aes(bases.per.problem, errors,
                       color=chunks, size=chunks),
                   data=one.error)+
         geom_line(aes(bases.per.problem, errors,
                       color=chunks, size=chunks),
                   data=two.error),

    problems=ggplot()+
         ggtitle("select problem")+
         geom_segment(aes(problemStart, problem.i,
                          xend=problemEnd, yend=problem.i),
                      clickSelects="problem.name",
                      showSelected="bases.per.problem",
                      size=5,
                      data=data.frame(problems, sample.id="problems"))+
         geom_text(aes(200, 5,
                       label=paste("problem size", bases.per.problem)),
                   showSelected="bases.per.problem",
                   data=data.frame(sizes, sample.id="problems"))+
         geom_segment(aes(peakStart, problem.i,
                          xend=peakEnd, yend=problem.i),
                      showSelected=showSelected.vec,
                      clickSelects="problem.name",
                      data=data.frame(peak.problems, sample.id="problems"),
                      size=10,
                      color="deepskyblue")+
         ## TODO: yend=y=0 as params not aes?
         geom_segment(aes(peakStart, 0,
                          xend=peakEnd, yend=0),
                      showSelected=showSelected.vec,
                      clickSelects="problem.name",
                      data=samples,
                      size=10,
                      color="deepskyblue")+
         theme_bw()+
         theme(panel.margin=grid::unit(0, "cm"))+
         facet_grid(sample.id ~ .),

       title="viz with .variable .value",
       
       sizes=ggplot()+
         ggtitle("select problem size")+
         geom_point(aes(bases.per.problem, problems),
                    clickSelects="bases.per.problem",
                    size=10,
                    data=sizes),

       peaks=ggplot()+
         ggtitle("select number of peaks")+
         geom_point(aes(peaks, peaks,
                        color=error.type,
                        id=peaks),
                    showSelected=c("problem.name", "bases.per.problem"),
                    clickSelects = clickSelects.vec,
                    size=10,
                    data=peaks)+
         geom_text(aes(1, 3, label=problem.name),
                   showSelected=c("problem.name", "bases.per.problem"),
                   data=problems))

info <- animint2HTML(viz)

test_that("No widgets for .variable .value selectors", {
  computed.vec <- getSelectorWidgets(info$html)
  expected.vec <- c(
    "chunks", "problem.name", "bases.per.problem",
    "error.type")
  expect_identical(sort(computed.vec), sort(expected.vec))
})

circle.xpath <- '//svg[@id="plot_peaks"]//circle'
title.xpath <- paste0(circle.xpath, '//title')
tooltip.xpath <- '//div[@class="animint-tooltip"]'

test_that("animint-tooltip exists due to clickSelects", {
  tooltip_div <- getNodeSet(info$html, tooltip.xpath)
  expect_equal(length(tooltip_div), 1)
  # check that the tooltip has initial opactity 0
  opacity <- getStyleValue(getHTML(), tooltip.xpath, 'opacity')
  expect_identical(opacity, "0")
})

test_that("two lines rendered in first plot", {
  path.list <- getNodeSet(
    info$html, '//svg[@id="plot_errorLines"]//g[@class="PANEL1"]//path')
  style.strs <- sapply(path.list, function(x) xmlAttrs(x)["style"])
  pattern <-
    paste0("(?<name>\\S+?)",
           ": *",
           "(?<value>.+?)",
           ";")
  style.matrices <- str_match_all_perl(style.strs, pattern)
  size.vec <- sapply(style.matrices, function(m)m["stroke-width", "value"])
  size.num <- as.numeric(sub("px", "", size.vec))
  expect_equal(size.num, c(1, 2))
  color.vec <- sapply(style.matrices, function(m)m["stroke", "value"])
  expect_color(color.vec, c("red", "black"))
})

test_that(".variable and .value makes compiler create selectors", {
  selector.names <- sort(names(info$selectors))
  problem.selectors <- paste0(problems$problem.name)
  expected.names <-
    sort(c("problem.name",
           "error.type",
           "chunks",
           problem.selectors,
           "bases.per.problem"))
  expect_identical(selector.names, expected.names)
  selected <- sapply(info$selectors[problem.selectors], "[[", "selected")
  expect_true(all(selected == "1"))
})

test_that(".variable and .value renders correctly at first", {
  node.list <-
    getNodeSet(info$html, '//g[@class="geom6_segment_problems"]//line')
  expect_equal(length(node.list), 2)
})

test_that("clicking reduces the number of peaks", {
  no.peaks.html <- clickHTML(id=0)
  node.list <-
    getNodeSet(no.peaks.html, '//g[@class="geom6_segment_problems"]//line')
  expect_equal(length(node.list), 1)
})

test_that("clicking increases the number of peaks", {
  more.peaks.html <- clickHTML(id=2)
  node.list <-
    getNodeSet(more.peaks.html, '//g[@class="geom6_segment_problems"]//line')
  expect_equal(length(node.list), 3)
})

viz.for <-
  list(problems=ggplot()+
         ggtitle("select problem")+
         geom_segment(aes(problemStart, problem.i,
                          xend=problemEnd, yend=problem.i),
                      clickSelects="problem.name",
                      showSelected="bases.per.problem",
                      size=5,
                      data=data.frame(problems, sample.id="problems"))+
         geom_text(aes(200, 5,
                       label=paste("problem size", bases.per.problem)),
                   showSelected="bases.per.problem",
                   data=data.frame(sizes, sample.id="problems"))+
         theme_bw()+
         theme(panel.margin=grid::unit(0, "cm"))+
         facet_grid(sample.id ~ .),

       title="viz with for loop",
       
       sizes=ggplot()+
         ggtitle("select problem size")+
         geom_point(aes(bases.per.problem, problems),
                    clickSelects="bases.per.problem",
                    size=10,
                    data=sizes),

       peaks=ggplot()+
         ggtitle("select number of peaks")+
         geom_text(aes(1, 3, label=problem.name),
                   showSelected="problem.name",
                   data=problems))

pp.list <- split(peak.problems, peak.problems$problem.name)
s.list <- split(samples, samples$problem.name)
p.list <- split(peaks, peaks$problem.name)

for(problem.name in names(p.list)){
  s.name <- paste0(problem.name, "peaks")
  p <- p.list[[problem.name]]
  p[[s.name]] <- p$peaks
  pp <- pp.list[[problem.name]]
  pp[[s.name]] <- pp$peaks
  ## need problem.underscore since a proper CSS id (as recognized by
  ## phantomJS) does not have dots!
  pp$problem.nodots <- gsub("[.]", "", pp$problem.name)
  s <- s.list[[problem.name]]
  s[[s.name]] <- s$peaks
  p$bases.per.problem <- pp$bases.per.problem[1]
  
  viz.for$problems <- viz.for$problems+
    geom_segment(aes_string("peakStart", "problem.i",
                            id="problem.nodots",
                            xend="peakEnd", yend="problem.i"),
                 showSelected=c(s.name, "bases.per.problem"),
                 clickSelects="problem.name",
                 data=data.frame(pp, sample.id="problems"),
                 size=10,
                 color="deepskyblue")+
    geom_segment(aes_string("peakStart", "0",
                            xend="peakEnd", yend="0"),
                 showSelected=c(s.name, "bases.per.problem"),
                 clickSelects="problem.name",
                 data=s,
                 size=10,
                 color="deepskyblue")
  viz.for$peaks <- viz.for$peaks+
         geom_point(aes_string("peaks", "peaks"),
                    showSelected=c("problem.name", "bases.per.problem"),
                    clickSelects=s.name,
                    size=10,
                    data=p)
}

info <- animint2HTML(viz.for)

test_that("Widgets for regular selectors", {
  computed.vec <- getSelectorWidgets(info$html)
  expected.vec <- c(
    "problem.name", "bases.per.problem",
    "size.100.problem.1peaks", "size.100.problem.2peaks",
    "size.50.problem.1peaks", "size.50.problem.2peaks", 
    "size.50.problem.3peaks", "size.50.problem.4peaks")
  expect_identical(sort(computed.vec), sort(expected.vec))
})

chunk.counts <- function(html=getHTML()){
  node.set <-
    getNodeSet(html, '//td[@class="downloaded"]')
  as.integer(sapply(node.set, xmlValue))
}

test_that("counts of chunks downloaded or not at first", {
  value.vec <- chunk.counts()
  expect_equal(value.vec,
               c(1, 1, 1, 1, 1, 1,
                 0, 0, 0, 0, 0, 0, 0, 0,
                 1, 1, 1,
                 0, 0, 0, 0, 0))
})

test_that("changing problem downloads one chunk", {
  clickID('size100problem2')
  Sys.sleep(1)
  value.vec <- chunk.counts()
  expect_equal(value.vec,
               c(1, 1, 1, 1, 1, 1,
                 0, 0, 0, 0, 0, 0, 0, 0,
                 1, 1, 1, 1,
                 0, 0, 0, 0))
})
test_that("animint-tooltip exists due to clickSelects", {
  tooltip_div <- getNodeSet(info$html, tooltip.xpath)
  expect_equal(length(tooltip_div), 1)
  # check that the tooltip has initial opactity 0
  opacity <- getStyleValue(getHTML(), tooltip.xpath, 'opacity')
  expect_identical(opacity, "0")
})
