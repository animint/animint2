data(parallelPeaks,package="animint2")
library(animint2)
library(data.table)
max.proc <- max(parallelPeaks$process_i)
ggplot()+
  theme_bw()+
  geom_segment(aes(
    start.seconds, process_i,
    color=design,
    xend=end.seconds, yend=process_i),
    data=parallelPeaks)+
  geom_point(aes(
    start.seconds, process_i, color=design),
    shape=1,
    data=parallelPeaks)+
  facet_grid(Fun + design ~ ., scales="free", labeller=label_both)+
  scale_y_continuous(breaks=seq(1, max.proc))+
  scale_x_continuous("Time from start of computation (seconds)")

gg_wrap <- ggplot()+
  theme_bw()+
  geom_segment(aes(
    start.seconds, process_i,
    xend=end.seconds, yend=process_i),
    data=parallelPeaks)+
  geom_point(aes(
    start.seconds, process_i),
    shape=1,
    data=parallelPeaks)+
  geom_blank(aes(
    start.seconds, process_i-1),
    shape=1,
    data=parallelPeaks)+
  geom_label_aligned(aes(
    start.seconds, process_i-0.5, label=peaks),
    alignment = "horizontal",
    data=parallelPeaks)+
  facet_wrap( ~ design + Fun, labeller=label_both, ncol=1)+
  scale_y_continuous(
    breaks=c(1,5,8,14))+
  scale_x_continuous("Time from start of computation (seconds)")
animint(gg_wrap+theme_animint(width=1500, height=2000))

animint(gg_wrap+theme_animint(width=1500, height=1000))
