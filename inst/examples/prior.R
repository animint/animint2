library(animint2)
data(prior)

prior$accuracy$percent <- prior$accuracy$accuracy.mean * 100
prior$accuracy$percent.se <- prior$accuracy$accuracy.se * 100
sqLab <- "squared error of the prior estimate"
priorBands <-
  list(set=a_plot()+
       a_geom_abline()+
       a_geom_text(a_aes(positive, negative, a_label=set), data=prior$data)+
       a_geom_point(a_aes(positive, negative, size=dimension),
                  clickSelects="set",
                  data=prior$data)+
       a_scale_size_continuous(range=c(3,20),breaks=prior$data$dim),
       error=a_plot()+
       make_text(prior$accuracy, 86, 0.3, "prior")+
       make_text(prior$accuracy, 86, 0.32, "samples")+
       a_geom_point(a_aes(percent, sqErr.mean, fill=method, colour=classifier),
                  showSelected=c("prior", "samples"),
                  clickSelects="set",
                  data=prior$accuracy, size=4)+
       a_scale_colour_manual(values=c("Kernel logistic regression"="black",
                             "Least squares probabalistic classifier"="white"))+
       ylab(sqLab)+
       xlab("percent classification accuracy"),
       samples=a_plot()+
       make_tallrect(prior$accuracy, "samples")+
       make_text(prior$accuracy, 175, 97.5, "prior")+
       make_text(prior$accuracy, 175, 95, "set")+
       a_geom_ribbon(a_aes(samples,
                       ymin=percent-percent.se,
                       ymax=percent+percent.se,
                       group=interaction(method, classifier),
                       fill=method),
                   showSelected=c("prior", "set"),
                   data=prior$accuracy, alpha=1/4)+
       a_geom_line(a_aes(samples, percent, group=interaction(method, classifier),
                     colour=method, linetype=classifier),
                 showSelected=c("prior", "set"),
                 data=prior$accuracy)+
       a_guides(colour="none",linetype="none",fill="none")+
       xlab("number of points sampled")+
       ylab("percent classification accuracy"),
       prior=a_plot()+
       make_tallrect(prior$accuracy, "prior")+
       make_text(prior$accuracy, 0.5, 97.5, "samples")+
       make_text(prior$accuracy, 0.5, 95, "set")+
       a_geom_ribbon(a_aes(prior, ymin=percent-percent.se, ymax=percent+percent.se,
                       group=interaction(method, classifier),
                       fill=method),
                   showSelected=c("samples", "set"),
                   data=prior$accuracy, alpha=1/4)+
       a_geom_line(a_aes(prior, percent, group=interaction(method, classifier),
                     colour=method, linetype=classifier),
                 showSelected=c("samples", "set"),
                 data=prior$accuracy)+
       xlab("class prior")+
       ylab("percent classification accuracy"),
       samplessqErr=a_plot()+
       make_tallrect(prior$accuracy, "samples")+
       a_geom_ribbon(a_aes(samples,
                       ymin=sqErr.mean-sqErr.se,
                       ymax=sqErr.mean+sqErr.se,
                       group=interaction(method, classifier),
                       fill=method),
                   showSelected=c("prior", "set"),
                   data=prior$accuracy, alpha=1/4)+
       a_geom_line(a_aes(samples, sqErr.mean, group=interaction(method, classifier),
                     colour=method, linetype=classifier),
                 showSelected=c("prior", "set"),
                 data=prior$accuracy)+
       a_guides(colour="none",linetype="none",fill="none")+
       xlab("number of points sampled")+
       ylab(sqLab),
       priorsqErr=a_plot()+
       make_tallrect(prior$accuracy, "prior")+
       a_geom_ribbon(a_aes(prior,
                       ymin=sqErr.mean-sqErr.se,
                       ymax=sqErr.mean+sqErr.se,
                       group=interaction(method, classifier),
                       fill=method),
                   showSelected=c("samples", "set"),
                   data=prior$accuracy, alpha=1/4)+
       a_geom_line(a_aes(prior, sqErr.mean, group=interaction(method, classifier),
                     colour=method, linetype=classifier),
                 showSelected=c("samples", "set"),
                 data=prior$accuracy)+
       xlab("class prior")+
       ylab(sqLab))
animint2dir(priorBands, "prior")

## are the exported files the same?

## csv.files <- Sys.glob("/tmp/RtmpVIt99h/filee8b6b741ce7/*.csv")
## for(i in 1:(length(csv.files)-1)){
##   for(j in (i+1):length(csv.files)){
##     cmd <- sprintf("diff %s %s|head -1",csv.files[i],csv.files[j])
##     out <- system(cmd, intern=TRUE)
##     if(length(out)==0){
##       print(cmd)
##     }
##   }
## }

## Answer: 3 pairs are the same: (12,20), (14,9), (17,7). So actually
## there is not so much repetition that can be easily avoided.
