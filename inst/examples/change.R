library(animint2)
data(change)

train.test <- data.frame(x=6, y=c(-3, -5), set=c("train","test"))
likPlot <- a_plot()+
  make_tallrect(change$lik, "complexity")+
  a_geom_line(a_aes(complexity, log(log.likelihood+.002), group=set, colour=set),
            data=change$lik, size=5)+
  xlab("model complexity -log(lambda)")+
  a_guides(colour="none")+
  a_geom_text(a_aes(x, y, a_label=set, colour=set), data=train.test)+
  ggtitle("Train and test likelihood")
print(likPlot)

## Just the variables which have really changed.
changed <- subset(change$truth, changed)
varPlot <- a_plot()+
  a_geom_text(a_aes(x,y,a_label=variable), data=change$pos)+
  a_geom_segment(a_aes(v1.x, v1.y, xend=v2.x, yend=v2.y,
                   size=change, colour=change),
               showSelected="complexity", clickSelects="variables",
               data=subset(change$coefs, coefficient != 0), alpha=3/4)+
  a_geom_segment(a_aes(v1.x, v1.y, xend=v2.x, yend=v2.y,
                   size=change, colour=change),
               data=changed)+
  scale_size_manual(values=c(guess=10, true=1))+
  scale_colour_manual(values=c(true="black",guess="violet"))+
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank())+
  ggtitle("Graphical model structure")
print(varPlot)

## The path of coefficients.
pathPlot <- a_plot()+
  make_tallrect(change$coefs, "complexity")+
  a_geom_line(a_aes(complexity, coefficient, group=variables, colour=truth),
            clickSelects="variables",
            data=change$coefs, alpha=3/4, size=3)+
  xlab("model complexity -log(lambda)")+
  ylab("norm of the difference between two variables")+
  scale_colour_manual(values=c("no change"="red", "change"="black"))+
  ggtitle("Regularization path")
print(pathPlot)

viz <- list(path=pathPlot, var=varPlot, lik=likPlot)
animint2dir(viz, "change")

