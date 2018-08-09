library(animint2)

## Example: 2 plots, 2 selectors, but only interacting with 1 plot.
data(breakpoints)
only.error <- subset(breakpoints$error,type=="E")
only.segments <- subset(only.error,bases.per.probe==bases.per.probe[1])
signal.colors <- c(estimate="#0adb0a",
                   latent="#0098ef")
breakpointError <- 
  list(signal=a_plot()+
         a_geom_point(a_aes(position, signal),
                    showSelected="bases.per.probe",
                    data=breakpoints$signals)+
         a_geom_line(a_aes(position, signal), colour=signal.colors[["latent"]],
                   data=breakpoints$imprecision)+
         a_geom_segment(a_aes(first.base, mean, xend=last.base, yend=mean),
                      showSelected=c("segments", "bases.per.probe"),
                      colour=signal.colors[["estimate"]],
                      data=breakpoints$segments)+
         a_geom_vline(a_aes(xintercept=base),
                    showSelected=c("segments", "bases.per.probe"),
                    colour=signal.colors[["estimate"]],
                    linetype="dashed",
                    data=breakpoints$breaks),
       error=a_plot()+
         a_geom_vline(a_aes(xintercept=segments), clickSelects="segments",
                    data=only.segments, lwd=17, alpha=1/2)+
         a_geom_line(a_aes(segments, error, group=bases.per.probe),
                   clickSelects="bases.per.probe",
                   data=only.error, lwd=4))
animint2dir(breakpointError)
