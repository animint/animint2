acontext("FigureLossSmall")
data("neuroblastoma", package="neuroblastoma")
library(data.table)
library(animint2) 

nb.dt <- data.table(neuroblastoma$profiles)

count.dt <- nb.dt[, list(
  same=sum(diff(logratio)==0),
  count=.N
), by=list(profile.id, chromosome)]
count.dt[order(same, -count)][count<1000]
count.dt[order(count)]
## Third data set, really small.
## First profile has some consecutive data points that are the same.
three <- nb.dt[profile.id=="371"&chromosome=="Y"]
sum(diff(three$logratio)==0)
max.segments <- nrow(three)
fit <- jointseg::Fpsn(three$logratio, max.segments)
loss.dt <- with(fit, data.table(
  segments=1:max.segments,
  loss=J.est))
selected <- penaltyLearning::modelSelection(loss.dt, complexity="segments")
loss.dt[, selected := ifelse(segments %in% selected$segments, "yes", "no")]
loss.dt[, table(selected)]
loss.dt[, table(selected)/nrow(loss.dt)]

small.dt <- count.dt[count<1000]
nb.small <- nb.dt[small.dt, on=list(profile.id, chromosome)]
loss.small <- nb.small[, {
  max.segments <- .N
  fit <- jointseg::Fpsn(logratio, max.segments)
  loss.dt <- with(fit, data.table(
    changes=(1:max.segments)-1,
    loss=J.est))
  selected <- penaltyLearning::modelSelection(loss.dt, complexity="changes")
  loss.dt[, selected := ifelse(changes %in% selected$changes, "yes", "no")]
  loss.dt
}, by=list(profile.id, chromosome)]



prop.dt <- loss.small[, {
  n.selected <- sum(selected=="yes")
  data.table(
    n.selected,
    max.loss=max(loss),
    prop.selected=n.selected/.N,
    models=.N)
}, by=list(profile.id, chromosome)]

set.seed(100)
(some.props <- prop.dt[
  sample(1:.N, 5)][, .SD[1], by=list(models, prop.selected)])

some.loss <- loss.small[some.props, on=list(profile.id, chromosome)]
some.nb <- nb.dt[some.props, on=list(profile.id, chromosome)]
some.segs <- some.nb[, {
  fit <- jointseg::Fpsn(logratio, .N)
  seg.list <- list()
  for(model.i in 1:.N){
    end.vec <- fit$t.est[model.i, 1:model.i]
    start.vec <- c(1, end.vec[-model.i]+1)
    for(seg.i in 1:model.i){
      start <- start.vec[[seg.i]]
      end <- end.vec[[seg.i]]
      seg.list[[paste(model.i, seg.i)]] <- data.table(
        start,
        end,
        changes=model.i-1,
        segStart=position[start],
        segEnd=position[end],
        mean=mean(logratio[start:end]))
    }
  }
  do.call(rbind, seg.list)
}, by=list(profile.id, chromosome)]
some.selection <- some.loss[selected=="yes", {
  penaltyLearning::modelSelection(.SD, complexity="changes")
}, by=list(profile.id, chromosome)]
some.breaks <- some.segs[0 < changes, list(
  megabases=(segStart[-1]+segEnd[-.N])/2e6
), by=list(profile.id, chromosome, changes)]
some.segs[, beforeStart := {
  c(
    segStart[1]-1,
    (segStart[-1]+segEnd[-.N])/2)/1e6
}, by=list(profile.id, chromosome, changes)]
some.segs[, afterEnd := {
  c(
    (segStart[-1]+segEnd[-.N])/2,
    segEnd[.N]+1)/1e6
}, by=list(profile.id, chromosome, changes)]

some.nb[, megabases := position/1e6]
some.nb[, pid.chr := paste0(profile.id, ".", chromosome)]
some.breaks[, pid.chr := paste0(profile.id, ".", chromosome)]
some.loss[, pid.chr := paste0(profile.id, ".", chromosome)]
some.loss[, penalty := 0]
some.loss[, min.changes := changes-0.5]
some.loss[, max.changes := changes+0.5]
some.props[, pid.chr := paste0(profile.id, ".", chromosome)]
some.selection[, pid.chr := paste0(profile.id, ".", chromosome)]
some.segs[, pid.chr := paste0(profile.id, ".", chromosome)]
some.selection[, cost.at.min.lambda := min.lambda*changes+loss]
vizWithUpdateAxes <- animint(
  title="Changepoint model selection",
  selected=ggplot()+
    theme_bw()+
    ggtitle("Click to select a data set")+
    scale_x_log10(
      "Number of data to segment",
      breaks=c(range(some.props$models), 10, 100))+
    ylab("Proportion of models selected by linear penalty")+
    geom_point(aes(
      models, prop.selected),
      shape=21,
      size=4,
      clickSelects="pid.chr",
      alpha=0.7,
      data=data.frame(some.props)),
  data=ggplot()+
    ggtitle("Selected data set and model")+
    theme_bw()+
    ##theme_animint(update_axes=c("x", "y"))+
    xlab("Position on chromosome (mega bases)")+
    ylab("logratio (approximate DNA copy number")+
    geom_segment(aes(
      beforeStart, mean,
      xend=afterEnd, yend=mean),
      showSelected=c("pid.chr", "changes"),
      color="green",
      data=data.frame(some.segs))+
    geom_vline(aes(
      xintercept=megabases),
      showSelected=c("pid.chr", "changes"),
      color="green",
      chunk_vars="pid.chr",
      linetype="dashed",
      data=data.frame(some.breaks))+
    geom_point(aes(
      megabases, logratio),
      shape=21,
      fill=NA,
      showSelected="pid.chr",
      data=data.frame(some.nb)),
  loss=ggplot()+
    ggtitle("Loss values for selected data set")+
    ylab("loss")+
    xlab("changes")+
    theme_bw()+
    theme_animint(update_axes=c("x", "y"))+
    scale_size_manual(values=c(yes=3, no=4))+
    geom_text(aes(
      models, max.loss, label=paste0(
        n.selected, "/", models,
        " models selected by linear penalty")),
      showSelected="pid.chr",
      hjust=1,
      data=data.frame(some.props))+
    geom_point(aes(
      changes, loss, color=selected, size=selected),
      shape=21,
      fill=NA,
      showSelected="pid.chr",
      data=data.frame(some.loss))+
    geom_tallrect(aes(
      xmin=min.changes, xmax=max.changes),
      data=data.frame(some.loss),
      alpha=0.5,
      showSelected="pid.chr",
      clickSelects="changes"),
  lines=ggplot()+
    ggtitle("Cost functions for selected data set")+
    theme_bw()+
    theme_animint(update_axes=c("x", "y"))+
    geom_point(aes(
      penalty, loss, color=selected),
      fill=NA,
      size=4,
      showSelected="pid.chr",
      data=data.frame(some.loss))+
    geom_point(aes(
      min.lambda, cost.at.min.lambda),
      showSelected="pid.chr",
      fill=NA,
      data=data.frame(some.selection))+
    ylab("cost(penalty) = loss + penalty*changes")+
    geom_abline(aes(
      slope=changes, intercept=loss, color=selected),
      size=1,
      showSelected="pid.chr",
      chunk_vars="pid.chr",
      data=data.frame(some.loss))+
    geom_abline(aes(
      slope=changes, intercept=loss, color=selected),
      size=2,
      showSelected=c("pid.chr", "changes"),
      chunk_vars="pid.chr",
      data=data.frame(some.loss))
)

infoWithUpdateAxes <- animint2HTML(vizWithUpdateAxes)

test_that("5 <circle> rendered for all data sets (with update axes)", {
  dataSets=getNodeSet(infoWithUpdateAxes$html, '//g[@class="geom1_point_selected"]//g//circle')
  expect_equal(length(dataSets), nrow(some.props))
})

test_that("72 <rect> rendered for selected data set (with update axes)", {
  nRect=getNodeSet(infoWithUpdateAxes$html, '//g[@class="geom7_tallrect_loss"]//g//rect')
  expect_equal(length(nRect), nrow(some.loss[some.loss$profile.id==322, ]))
})

vizWithoutUpdateAxes <- animint(
  title="Changepoint model selection",
  selected=ggplot()+
    theme_bw()+
    ggtitle("Click to select a data set")+
    scale_x_log10(
      "Number of data to segment",
      breaks=c(range(some.props$models), 10, 100))+
    ylab("Proportion of models selected by linear penalty")+
    geom_point(aes(
      models, prop.selected),
      shape=21,
      size=4,
      clickSelects="pid.chr",
      alpha=0.7,
      data=data.frame(some.props)),
  data=ggplot()+
    ggtitle("Selected data set and model")+
    theme_bw()+
    xlab("Position on chromosome (mega bases)")+
    ylab("logratio (approximate DNA copy number")+
    geom_segment(aes(
      beforeStart, mean,
      xend=afterEnd, yend=mean),
      showSelected=c("pid.chr", "changes"),
      color="green",
      data=data.frame(some.segs))+
    geom_vline(aes(
      xintercept=megabases),
      showSelected=c("pid.chr", "changes"),
      color="green",
      chunk_vars="pid.chr",
      linetype="dashed",
      data=data.frame(some.breaks))+
    geom_point(aes(
      megabases, logratio),
      shape=21,
      fill=NA,
      showSelected="pid.chr",
      data=data.frame(some.nb)),
  loss=ggplot()+
    ggtitle("Loss values for selected data set")+
    ylab("loss")+
    xlab("changes")+
    theme_bw()+
    scale_size_manual(values=c(yes=3, no=4))+
    geom_text(aes(
      models, max.loss, label=paste0(
        n.selected, "/", models,
        " models selected by linear penalty")),
      showSelected="pid.chr",
      hjust=1,
      data=data.frame(some.props))+
    geom_point(aes(
      changes, loss, color=selected, size=selected),
      shape=21,
      fill=NA,
      showSelected="pid.chr",
      data=data.frame(some.loss))+
    geom_tallrect(aes(
      xmin=min.changes, xmax=max.changes),
      data=data.frame(some.loss),
      alpha=0.5,
      showSelected="pid.chr",
      clickSelects="changes"),
  lines=ggplot()+
    ggtitle("Cost functions for selected data set")+
    theme_bw()+
    geom_point(aes(
      penalty, loss, color=selected),
      fill=NA,
      size=4,
      showSelected="pid.chr",
      data=data.frame(some.loss))+
    geom_point(aes(
      min.lambda, cost.at.min.lambda),
      showSelected="pid.chr",
      fill=NA,
      data=data.frame(some.selection))+
    ylab("cost(penalty) = loss + penalty*changes")+
    geom_abline(aes(
      slope=changes, intercept=loss, color=selected),
      size=1,
      showSelected="pid.chr",
      chunk_vars="pid.chr",
      data=data.frame(some.loss))+
    geom_abline(aes(
      slope=changes, intercept=loss, color=selected),
      size=2,
      showSelected=c("pid.chr", "changes"),
      chunk_vars="pid.chr",
      data=data.frame(some.loss))
)

infoWithoutUpdateAxes <- animint2HTML(vizWithUpdateAxes)

test_that("5 <circle> rendered for all data sets (without update axes)", {
  dataSets=getNodeSet(infoWithoutUpdateAxes$html, '//g[@class="geom1_point_selected"]//g//circle')
  expect_equal(length(dataSets), nrow(some.props))
})

test_that("72 <rect> rendered for selected data set (widhout update axes)", {
  nRect=getNodeSet(infoWithoutUpdateAxes$html, '//g[@class="geom7_tallrect_loss"]//g//rect')
  expect_equal(length(nRect), nrow(some.loss[some.loss$profile.id==322, ]))
})

