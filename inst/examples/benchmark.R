library(animint2)
library(reshape2)

data("uci621raw", package = "benchmark")
molt <- melt(uci621raw)
perfs <- dcast(molt,samp+alg+ds~perf)
perfMeans <- dcast(molt,alg+ds~perf,mean,na.rm=TRUE)
x <- 0.3
y <- 500
algos <-
  list(selectAlg=a_plot()+
       a_geom_point(a_aes(Misclassification, Time, colour=alg),
                  clickSelects="alg", showSelected="ds",
                  data=perfs, alpha=6/10)+
       make_text(perfs,x,y,"ds"),
       selectDS=a_plot()+
       ## a_geom_text(a_aes(Misclassification, Time, 
       ##                a_label=ds, showSelected=alg),
       ##            data=perfMeans)+
       a_geom_point(a_aes(Misclassification, Time, colour=alg),
                  clickSelects="ds", showSelected="alg",
                  data=perfMeans)+
       make_text(perfs,x,y,"alg"),
       selectAlgLog=a_plot()+
       a_geom_point(a_aes(Misclassification, log10(Time), colour=alg),
                  clickSelects="alg", showSelected="ds",
                  data=perfs, alpha=6/10)+
       make_text(perfs,x,log10(y),"ds"),
       selectDSLog=a_plot()+
       a_geom_text(a_aes(Misclassification, log10(Time), 
                      a_label=ds),
                 showSelected="alg",
                 data=perfMeans, alpha=1/4)+
       a_geom_point(a_aes(Misclassification, log10(Time), colour=alg),
                  clickSelects="ds", showSelected="alg",
                  data=perfMeans, size=5)+
       make_text(perfs,x,log10(y),"alg"))       
animint2dir(algos, "benchmark")
