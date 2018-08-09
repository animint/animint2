library(animint2)
data(prostateLasso)
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999")
hline.df <- data.frame(residual=0)
addY <- function(dt, y){
  data.frame(dt, y.var=factor(y, c("error", "weights")))
}
viz <- list(
  title="Lasso on the prostate cancer data set",
  path=a_plot()+
    a_theme_bw()+
    a_theme(panel.margin=grid::unit(0, "lines"))+
    a_facet_grid(y.var ~ ., scales="free")+
    ylab("")+
    a_scale_color_manual(values=variable.colors)+
    a_geom_line(a_aes(arclength, standardized.coef, color=variable, group=variable),
              data=addY(prostateLasso$path, "weights"))+
    a_geom_line(a_aes(arclength, mse, linetype=set, group=set),
              data=addY(prostateLasso$error, "error"))+
    make_tallrect(prostateLasso$error, "arclength"),
  res=a_plot()+
    a_geom_hline(a_aes(yintercept=residual),
               data=hline.df,
               color="grey")+
    a_guides(linetype="none")+
    a_geom_point(a_aes(response, residual, 
                   key=observation.i),
               showSelected=c("arclength", "set"),
               shape=21,
               fill=NA,
               color="black",
               data=prostateLasso$residuals)+
    a_geom_text(a_aes(3, 2.5, a_label=sprintf("L1 arclength = %.1f", arclength),
                  key=1),
              showSelected="arclength",
              data=prostateLasso$models)+
    a_geom_text(a_aes(0, -2, a_label=sprintf("train error = %.3f", mse),
                  key=1),
              showSelected=c("set", "arclength"),
              hjust=0,
              data=subset(prostateLasso$error, set=="train"))+
    a_geom_text(a_aes(0, -2.5, a_label=sprintf("validation error = %.3f", mse),
                  key=1),
              showSelected=c("set", "arclength"),
              hjust=0,
              data=subset(prostateLasso$error, set=="validation"))+
    a_geom_segment(a_aes(response, residual,
                     xend=response, yend=0,
                     linetype=set,
                     key=observation.i),
                 showSelected=c("set", "arclength"),
                 data=prostateLasso$residuals),
  duration=list(arclength=2000))
animint2dir(viz, "figure-prostateLasso")

