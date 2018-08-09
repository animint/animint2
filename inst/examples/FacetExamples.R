library(animint2)
library(plyr)

#' density: should show two normal distributions, centered at 0 and 3, and a gamma distribution with mode approximately 5
boxplotdata <- rbind(data.frame(x=1:50, y=sort(rnorm(50, 3, 1)), group="N(3,1)"),
                     data.frame(x=1:50, y=sort(rnorm(50, 0, 1)), group="N(0,1)"), 
                     data.frame(x=1:50, y=sort(rgamma(50, 2, 1/3)), group="Gamma(2,1/3)"))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax=max(y), ymin=min(y), med=median(y))

f1 <- a_plot() + a_geom_density(data=boxplotdata, a_aes(x=y, group=group, fill=group), alpha=.5) +
  a_scale_fill_discrete("Distribution") + xlab("x") + 
  ggtitle("a_geom_density") + a_facet_wrap(~group)
f1

temp <- a_plot_build(f1)