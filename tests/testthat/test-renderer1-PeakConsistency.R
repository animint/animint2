acontext("PeakConsistency")

data(PeakConsistency, package = "animint2")

color.code <-
  c(truth="#1B9E77", #teal
    PeakSeg="#D95F02", #orange
    PeakSegJoint="#7570B3", #violet
    "#E7298A", #pink
    "#66A61E", #green
    "#E6AB02", #tan
    "#A6761D", #brown
    "#666666") #grey

second.small <-
  list(signals=a_plot()+
         a_theme_bw()+
         a_theme_animint(width=1000, height=800)+
         a_theme(panel.margin=grid::unit(0, "cm"))+
         a_facet_grid(sample.id ~ ., labeller=function(val){
           mapply(paste, "sample", val, SIMPLIFY = FALSE)
         })+
         a_guides(size="none")+
         a_geom_segment(a_aes(chromStart+0.5, mean,
                          xend=chromEnd+0.5, yend=mean,
                          color=model, size=model),
                      showSelected=c("seed", "sample.size"),
                      data=subset(PeakConsistency$model, increase==1))+
         a_scale_size_manual(values=c(PeakSegJoint=0.5, PeakSeg=1))+
         a_scale_color_manual(values=color.code),
       first=list(sample.size=5))

info <- animint2HTML(second.small)

getStroke <- function(element.list){
  style.strs <- sapply(element.list, function(x) xmlAttrs(x)["style"])
  pattern <-
    paste0("(?<name>\\S+?)",
           ": *",
           "(?<value>.+?)",
           ";")
  style.matrices <- str_match_all_perl(style.strs, pattern)
  sapply(style.matrices, function(m)m["stroke", "value"])
}

test_that("15 segments of both colors", {
  line.list <-
    getNodeSet(info$html, '//g[@class="a_geom1_segment_signals"]//line')
  computed.vec <- getStroke(line.list)
  color.counts <- as.numeric(table(computed.vec))
  expect_equal(color.counts, c(15, 15))
})

viz <-
  list(increase=a_plot()+
         make_tallrect(PeakConsistency$increase, "increase")+
         a_geom_line(a_aes(increase, mean.diff), data=PeakConsistency$increase),
       errors=a_plot()+
         ylab("distance from true peaks to estimated peaks")+
         a_scale_color_manual(values=color.code)+
         make_tallrect(PeakConsistency$error, "sample.size")+
         a_geom_line(a_aes(sample.size, errors,
                       group=interaction(model, seed),
                       color=model),
                   showSelected="increase",
                   clickSelects="seed",
                   size=5,
                   alpha=0.7,
                   data=PeakConsistency$error),
       signals=a_plot()+
         a_theme_bw()+
         a_theme_animint(width=1000, height=800)+
         a_theme(panel.margin=grid::unit(0, "cm"))+
         a_facet_grid(sample.id ~ ., labeller=function(val){
           mapply(paste, "sample", val, SIMPLIFY = FALSE)
         })+
         a_geom_point(a_aes(chromEnd, count),
                    showSelected=c("seed", "increase"),
                    color="grey50",
                    data=PeakConsistency$signal)+
         a_geom_vline(a_aes(xintercept=chromStart+0.5, color=model),
                    showSelected=c("increase", "seed"),
                    show.legend=TRUE,
                    linetype="dashed",
                    data=PeakConsistency$truth)+
         a_guides(size="none")+
         a_geom_segment(a_aes(chromStart+0.5, mean,
                          xend=chromEnd+0.5, yend=mean,
                          color=model, size=model),
                      showSelected=c("seed", "sample.size", "increase"),
                      data=PeakConsistency$model)+
         a_geom_vline(a_aes(xintercept=chromStart+0.5,
                        color=model, size=model),
                    showSelected=c("seed", "sample.size", "increase"),
                    show.legend=TRUE,
                    linetype="dashed",
                    data=PeakConsistency$guess)+
         a_scale_size_manual(values=c(PeakSegJoint=1, PeakSeg=2))+
         a_scale_color_manual(values=color.code),
       first=list(sample.size=5))

## viz$errors+a_facet_grid(. ~ increase)
## viz$signals+a_facet_grid(sample.id ~ increase + seed)

info <- animint2HTML(viz)

test_that("4 paths of both colors in second plot", {
  path.list <- 
    getNodeSet(info$html, '//g[@class="a_geom4_line_errors"]//path')
  computed.vec <- getStroke(path.list)
  color.counts <- as.numeric(table(computed.vec))
  expect_equal(color.counts, c(4, 4))
})

test_that("15 segments of both colors in last plot", {
  line.list <-
    getNodeSet(info$html, '//g[@class="a_geom7_segment_signals"]//line')
  computed.vec <- getStroke(line.list)
  color.counts <- as.numeric(table(computed.vec))
  expect_equal(color.counts, c(15, 15))
})

## test_that("showSelectedlegendcolour is truth", {
##   tsv.path <-
##     file.path("animint-htmltest", "a_geom6_vline_signals_chunk_common.tsv")
##   common.df <- read.table(tsv.path, comment.char="", header=TRUE)
##   computed.vec <- paste(common.df$showSelectedlegendcolour)
##   expected.vec <- rep("truth", length(computed.vec))
##   expect_identical(computed.vec, expected.vec)
##   tsv.path <-
##     file.path("animint-htmltest", "a_geom6_vline_signals_chunk1.tsv")
##   varied.df <- read.table(tsv.path, comment.char="", header=TRUE)
## })

test_that("20 truth <line> in last plot", {
  line.list <-
    getNodeSet(info$html, '//g[@class="a_geom6_vline_signals"]//line')
  expect_equal(length(line.list), 20)
})

test_that("20 prediction <line> in last plot", {
  line.list <-
    getNodeSet(info$html, '//g[@class="a_geom8_vline_signals"]//line')
  expect_equal(length(line.list), 20)
})

