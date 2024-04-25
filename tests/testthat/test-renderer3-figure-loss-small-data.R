acontext("FigureLossSmall")
library(data.table)
library(animint2)

loss.small <- readRDS("loss.small.rds")

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
ggplot()+
  scale_x_log10(
    "Number of data to segment",
    breaks=c(range(some.props$models), 10, 100))+
  geom_point(aes(
    models, prop.selected),
    shape=21,
    data=some.props)

some.loss <- loss.small[some.props, on=list(profile.id, chromosome)]
some.nb <- readRDS("nb.rds")
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
viz <- animint(
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
info <- animint2HTML(viz)

test_that("5 <circle> rendered for all data sets", {
  dataSets=getNodeSet(info$html, '//g[@class="geom1_point_selected"]//g//circle')
  expect_equal(length(dataSets), 5)
})

test_that("72 <rect> rendered for selected data set", {
  nRect=getNodeSet(info$html, '//g[@class="geom7_tallrect_loss"]//g//rect')
  expect_equal(length(nRect), 72)
})

