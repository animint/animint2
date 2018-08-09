library(testthat)
acontext("a_geom line")

data(intreg, package = "animint2")
min.logratio <- min(intreg$signals$logratio)-0.2
max.logratio <- max(intreg$signals$logratio)
intreg$breaks$min.logratio <- min.logratio
intreg$breaks$max.logratio <- max.logratio
signal.colors <- c(estimate="#0adb0a", latent="#0098ef")
breakpoint.colors <- c("1breakpoint"="#ff7d7d", "0breakpoints"='#f6f4bf')
models.by.signal <- with(intreg, split(segments, segments$signal))
signals.by.signal <- with(intreg, split(signals, signals$signal))
model.selection.list <- list()
sig.a_labels.list <- list()
sig.seg.names.list <- list()
sig.names.list <- list()
for(signal.name in names(models.by.signal)){
  signal <- signals.by.signal[[signal.name]]
  signal.models <- models.by.signal[[signal.name]]
  models.by.segments <- split(signal.models, signal.models$segments)
  sig.seg.by.segments <- list()
  for(segments.str in names(models.by.segments)){
    segments <- as.numeric(segments.str)
    model <- models.by.segments[[segments.str]]
    model$rss <- NA
    model$data <- NA
    for(segment.i in 1:nrow(model)){
      one.segment <- model[segment.i, ]
      seg.data <- 
        subset(signal,
               one.segment$first.base < base &
                 base < one.segment$last.base)
      model$data[segment.i] <- nrow(seg.data)
      residual.vec <- seg.data$logratio - one.segment$mean
      model$rss[segment.i] <- sum(residual.vec * residual.vec)
    }
    stopifnot(sum(model$data) == nrow(signal))
    model.stats <- 
      data.frame(signal=signal.name,
                 segments,
                 error=sum(model$rss),
                 data=sum(model$data))
    model.selection.list[[paste(signal.name, segments.str)]] <- model.stats
    if(segments.str == "1"){
      sig.a_labels.list[[signal.name]] <- model.stats
    }
    sig.seg.by.segments[[segments.str]] <-
      data.frame(signal=signal.name,
                 segments, min.logratio, max.logratio,
                 base=(min(signal$base)+max(signal$base))/2)
  }
  sig.seg.tall <- do.call(rbind, sig.seg.by.segments)
  sig.seg.names.list[[signal.name]] <- sig.seg.tall
  sig.names.list[[signal.name]] <- sig.seg.tall[1,]
}
model.selection.sorted <- do.call(rbind, model.selection.list)
set.seed(1)
model.selection <-
  model.selection.sorted[sample(1:nrow(model.selection.sorted)),]
sig.seg.names <- do.call(rbind, sig.seg.names.list)
sig.names <- do.call(rbind, sig.names.list)
sig.a_labels <- do.call(rbind, sig.a_labels.list)

## Plot segments rather than penalty.
mmir.selection <- 
  list(error=a_plot()+
       ggtitle("Select profile and number of segments")+
       make_tallrect(model.selection, "segments",
                     colour=signal.colors[["estimate"]])+
       a_theme_bw()+
       a_theme_animint(width=600)+
       a_theme(panel.margin=grid::unit(0, "lines"))+
       a_facet_grid(. ~ a_geom)+
       a_geom_text(a_aes(0, error, a_label=signal),
                 clickSelects="signal",
                 data=sig.a_labels, hjust=1)+
       a_scale_x_continuous("segments", breaks=c(1, 5, 10, 20),
                          limits=c(-2, 20))+
       xlab("squared error")+
       a_geom_line(a_aes(segments, error,
                     group=signal),
                 data=data.frame(model.selection, a_geom="line"),
                 clickSelects="signal",
                 alpha=0.6, size=8)+
       a_geom_path(a_aes(segments, error,
                     group=signal),
                 data=data.frame(model.selection, a_geom="path"),
                     clickSelects="signal",
                 alpha=0.6, size=8),

       signal=a_plot()+
         a_theme_bw()+
       a_theme_animint(width=800)+       
       a_scale_x_continuous("position on chromosome (mega base pairs)",
                          breaks=c(100,200))+
       a_scale_fill_manual(values=breakpoint.colors,a_guide="none")+
       a_geom_blank(a_aes(first.base/1e6, logratio+2/8), data=intreg$ann)+
       ggtitle("Copy number profile and maximum likelihood segmentation")+
       ylab("logratio")+
       a_geom_point(a_aes(base/1e6, logratio),
                  data=intreg$sig,
                      showSelected="signal")+
       a_geom_segment(a_aes(first.base/1e6, mean, xend=last.base/1e6, yend=mean),
                    data=intreg$seg, colour=signal.colors[["estimate"]],
                        showSelected=c("signal", "segments"))+
       a_geom_segment(a_aes(base/1e6, min.logratio,
                        xend=base/1e6, yend=max.logratio),
                  colour=signal.colors[["estimate"]],
                    showSelected=c("signal", "segments"),
                  linetype="dashed",
                  data=intreg$breaks)+
       a_geom_text(a_aes(base/1e6, max.logratio, a_label=paste("signal", signal)),
                 data=sig.names,
                 showSelected="signal")+
       a_geom_text(a_aes(base/1e6, min.logratio,
                     a_label=sprintf("%d segment%s", segments,
                       ifelse(segments==1, "", "s"))),
                 data=sig.seg.names,
                 showSelected=c("signal", "segments"),
                 color=signal.colors[["estimate"]]),

       first=list(signal="4.2", segments=4))

animint2dir(mmir.selection, "intreg-selection", open.browser=FALSE)

all.increasing <- function(num.vec){
  stopifnot(is.numeric(num.vec))
  all(0 < diff(num.vec))
}
expected.list <-
  list(a_geom3_line_error=TRUE,
       a_geom4_path_error=FALSE)
result.list <- list()
for(g.class in names(expected.list)){
  expected <- expected.list[[g.class]]
  tsv.path <- Sys.glob(file.path("intreg-selection", paste0(g.class, "*")))
  g.data <- read.table(tsv.path, header=TRUE, comment.char = "")
  tsv.by.signal <- split(g.data, g.data$clickSelects)
  for(signal.name in names(tsv.by.signal)){
    one.signal <- tsv.by.signal[[signal.name]]
    computed <- all.increasing(one.signal$x)
    result.list[[paste(g.class, signal.name)]] <-
      data.frame(g.class, signal.name, computed, expected)
  }
}
result <- do.call(rbind, result.list)

test_that("line sorts tsv data by x value, path does not", {
  with(result, {
    expect_identical(computed, expected)
  })
})
