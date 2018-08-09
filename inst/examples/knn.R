library(ElemStatLearn)
data(mixture.example)
str(mixture.example)
library(MASS)
library(data.table)
set.seed(123)
centers <- c(sample(1:10, 5000, replace=TRUE), 
               sample(11:20, 5000, replace=TRUE))
mix.test <- mvrnorm(10000, c(0,0), 0.2*diag(2))
test.points <- data.table(
  mix.test + mixture.example$means[centers,],
  a_label=factor(c(rep(0, 5000), rep(1, 5000))))
pred.grid <- data.table(mixture.example$xnew, a_label=NA)
input.cols <- c("V1", "V2")
names(pred.grid)[1:2] <- input.cols
test.and.grid <- rbind(
  data.table(test.points, set="test"),
  data.table(pred.grid, set="grid"))
test.and.grid$fold <- NA
test.and.grid
n.folds <- 10
set.seed(2)
mixture <- with(mixture.example, data.table(x, a_label=factor(y)))
mixture$fold <- sample(rep(1:n.folds, l=nrow(mixture)))
OneFold <- function(validation.fold){
  require(class)
  set <- ifelse(mixture$fold == validation.fold, "validation", "train")
  fold.data <- rbind(test.and.grid, data.table(mixture, set))
  fold.data$data.i <- 1:nrow(fold.data)
  only.train <- subset(fold.data, set == "train")
  data.by.neighbors <- list()
  for(neighbors in seq(1, 30, by=2)){
    cat(sprintf("n.folds=%4d validation.fold=%d neighbors=%d\n",
                n.folds, validation.fold, neighbors))
    pred.a_label <- 
      knn(only.train[, input.cols, with=FALSE],
          fold.data[, input.cols, with=FALSE],
          only.train$a_label,
          k=neighbors,
          prob=TRUE)
    prob.winning.class <- attr(pred.a_label, "prob")
    fold.data$probability <- ifelse(
      pred.a_label=="1", prob.winning.class, 1-prob.winning.class)
    fold.data[, pred.a_label := ifelse(0.5 < probability, "1", "0")]
    fold.data[, is.error := a_label != pred.a_label]
    fold.data[, prediction := ifelse(is.error, "error", "correct")]
    data.by.neighbors[[paste(neighbors)]] <- 
      data.table(neighbors, fold.data)
  }#for(neighbors
  do.call(rbind, data.by.neighbors)
}#for(validation.fold
library(doParallel)
registerDoParallel()
data.all.folds <- foreach(validation.fold=0:n.folds, .combine=rbind) %dopar% {
  one.fold <- OneFold(validation.fold)
  data.table(validation.fold, one.fold)
}
data.all.folds
labeled.data <- data.all.folds[!is.na(a_label),]
error.stats <- labeled.data[, list(
  error.prop=mean(is.error)
  ), by=.(set, validation.fold, neighbors)]
validation.error <- error.stats[set=="validation", list(
  mean=mean(error.prop),
  sd=sd(error.prop)/sqrt(.N)
  ), by=.(set, neighbors)]
validation.error
Bayes.error <- data.table(
  set="Bayes",
  validation.fold=NA,
  neighbors=NA,
  error.prop=0.21)
Bayes.error
other.error <- error.stats[validation.fold==0,]
head(other.error)
set.colors <- c(
  test="#377EB8", #blue
  validation="#4DAF4A",#green
  Bayes="#984EA3",#purple
  train="#FF7F00")#orange
classifier.linetypes <- c(
  Bayes="dashed",
  KNN="solid")
set.linetypes <- set.colors
set.linetypes[] <- classifier.linetypes[["KNN"]]
set.linetypes["Bayes"] <- classifier.linetypes[["Bayes"]]
cbind(set.linetypes, set.colors)

errorPlot <- a_plot()+
  a_theme_bw()+
  a_geom_hline(a_aes(yintercept=error.prop, color=set, linetype=set),
             data=Bayes.error)+
  a_scale_color_manual(
    "error type", values=set.colors, breaks=names(set.colors))+
  a_scale_linetype_manual(
    "error type", values=set.linetypes, breaks=names(set.linetypes))+
  ylab("Misclassification Errors")+
  xlab("Number of Neighbors")+
  a_geom_linerange(a_aes(neighbors, ymin=mean-sd, ymax=mean+sd,
                     color=set),
                 data=validation.error)+
  a_geom_line(a_aes(neighbors, mean, linetype=set, color=set),
            data=validation.error)+
  a_geom_line(a_aes(neighbors, error.prop, group=set, linetype=set, color=set),
            data=other.error)+
  a_geom_point(a_aes(neighbors, mean, color=set),
             data=validation.error)+
  a_geom_point(a_aes(neighbors, error.prop, color=set),
             data=other.error)
errorPlot
show.neighbors <- 7
show.data <- data.all.folds[validation.fold==0 & neighbors==show.neighbors,]
show.points <- show.data[set=="train",]
show.points
text.height <- 0.25
text.V1.prop <- 0
text.V2.bottom <- -2
text.V1.error <- -2.6
error.text <- rbind(
  Bayes.error,
  other.error[neighbors==show.neighbors,])
error.text[, V2.top := text.V2.bottom + text.height * (1:.N)]
error.text[, V2.bottom := V2.top - text.height]
error.text
getBoundaryDF <- function(prob.vec){
  stopifnot(length(prob.vec) == 6831)
  several.paths <- with(mixture.example, contourLines(
    px1, px2,
    matrix(prob.vec, length(px1), length(px2)),
    levels=0.5))
  contour.list <- list()
  for(path.i in seq_along(several.paths)){
    contour.list[[path.i]] <- with(several.paths[[path.i]], data.table(
      path.i, V1=x, V2=y))
  }
  do.call(rbind, contour.list)
}
boundary.grid <- show.data[set=="grid",]
boundary.grid[, a_label := pred.a_label]
pred.boundary <- getBoundaryDF(boundary.grid$probability)
pred.boundary$classifier <- "KNN"
Bayes.boundary <- getBoundaryDF(mixture.example$prob)
Bayes.boundary$classifier <- "Bayes"
Bayes.boundary
on.text <- function(V1, V2){
  V2 <= max(error.text$V2.top) & V1 <= text.V1.prop
}
show.grid <- boundary.grid[!on.text(V1, V2),]
show.grid
a_label.colors <- c(
  "0"="#377EB8",
  "1"="#FF7F00")
scatterPlot <- a_plot()+
  a_theme_bw()+
  a_theme(axis.text=a_element_blank(),
        axis.ticks=a_element_blank(),
        axis.title=a_element_blank())+
  ggtitle("7-Nearest Neighbors")+
  a_scale_color_manual(values=a_label.colors)+
  a_scale_linetype_manual(values=classifier.linetypes)+
  a_geom_point(a_aes(V1, V2, color=a_label),
             size=0.2,
             data=show.grid)+
  a_geom_path(a_aes(V1, V2, group=path.i, linetype=classifier),
            size=1,
            data=pred.boundary)+
  a_geom_path(a_aes(V1, V2, group=path.i, linetype=classifier),
            color=set.colors[["Bayes"]],
            size=1,
            data=Bayes.boundary)+
  a_geom_point(a_aes(V1, V2, color=a_label),
             fill=NA,
             size=3,
             shape=21,
             data=show.points)+
  a_geom_text(a_aes(text.V1.error, V2.bottom, a_label=paste(set, "Error:")),
            data=error.text,
            hjust=0)+
  a_geom_text(a_aes(text.V1.prop, V2.bottom, a_label=sprintf("%.3f", error.prop)),
            data=error.text,
            hjust=1)
scatterPlot
viz.static <- list(
  error=errorPlot,
  data=scatterPlot
  )
library(animint2)
animint2dir(viz.static, "knn-static")
Bayes.segment <- data.table(
  Bayes.error,
  classifier="Bayes",
  min.neighbors=1,
  max.neighbors=29)
Bayes.segment$set <- "test"
validation.error$classifier <- "KNN"
other.error$classifier <- "KNN"
set.colors <-
  c(test="#984EA3",#purple
    validation="#4DAF4A",#green
    Bayes="#984EA3",#purple
    train="black")
errorPlot <- a_plot()+
  ggtitle("Select number of neighbors")+
  a_theme_bw()+
  a_theme_animint(height=500)+
  a_geom_text(a_aes(min.neighbors, error.prop,
                color=set, a_label="Bayes"),
            showSelected="classifier",
            hjust=1,
            data=Bayes.segment)+
  a_geom_segment(a_aes(min.neighbors, error.prop, 
                   xend=max.neighbors, yend=error.prop,
                   color=set, linetype=classifier),
               showSelected="classifier",
               data=Bayes.segment)+
  a_scale_color_manual(values=set.colors, breaks=names(set.colors))+
  a_scale_fill_manual(values=set.colors)+
  a_guides(fill="none", linetype="none")+
  a_scale_linetype_manual(values=classifier.linetypes)+
  ylab("Misclassification Errors")+
  a_scale_x_continuous(
    "Number of Neighbors",
    limits=c(-1, 30),
    breaks=c(1, 10, 20, 29))+
  a_geom_ribbon(a_aes(neighbors, ymin=mean-sd, ymax=mean+sd,
                  fill=set),
              showSelected=c("classifier", "set"),
              alpha=0.5,
              data=validation.error)+
  a_geom_line(a_aes(neighbors, mean, color=set, linetype=classifier),
            showSelected="classifier",
            data=validation.error)+
  a_geom_line(a_aes(neighbors, error.prop, group=set, color=set,
                linetype=classifier),
            showSelected="classifier",
            data=other.error)+
  a_geom_tallrect(a_aes(xmin=neighbors-1, xmax=neighbors+1),
                clickSelects="neighbors",
                alpha=0.5,
                data=validation.error)
errorPlot
show.data <- data.all.folds[validation.fold==0,]
show.points <- show.data[set=="train",]
show.points
boundary.grid <- show.data[set=="grid",]
boundary.grid[, a_label := pred.a_label]
show.grid <- boundary.grid[!on.text(V1, V2),]
pred.boundary <- boundary.grid[, getBoundaryDF(probability), by=neighbors]
pred.boundary$classifier <- "KNN"
pred.boundary
show.text <- show.grid[, list(
  V1=mean(range(V1)), V2=3.05), by=neighbors]
other.error[, V2.bottom := text.V2.bottom + text.height * 1:2]
Bayes.error <- data.table(
  set="Bayes",
  error.prop=0.21)
scatterPlot <- a_plot()+
  ggtitle("Mis-classification errors in train set")+
  a_theme_bw()+
  a_theme_animint(width=500, height=500)+
  xlab("Input feature 1")+
  ylab("Input feature 2")+
  a_coord_equal()+
  a_scale_color_manual(values=a_label.colors)+
  a_scale_linetype_manual(values=classifier.linetypes)+
  a_geom_point(a_aes(V1, V2, color=a_label),
             showSelected="neighbors",
             size=0.2,
             data=show.grid)+
  a_geom_path(a_aes(V1, V2, group=path.i, linetype=classifier),
            showSelected="neighbors",
            size=1,
            data=pred.boundary)+
  a_geom_path(a_aes(V1, V2, group=path.i, linetype=classifier),
            color=set.colors[["test"]],
            size=1,
            data=Bayes.boundary)+
  a_geom_point(a_aes(V1, V2, color=a_label,
                 fill=prediction),
             showSelected="neighbors",
             size=3,
             shape=21,
             data=show.points)+
  a_scale_fill_manual(values=c(error="black", correct="transparent"))+
  a_geom_text(a_aes(text.V1.error, text.V2.bottom, a_label=paste(set, "Error:")),
            data=Bayes.error,
            hjust=0)+
  a_geom_text(a_aes(text.V1.prop, text.V2.bottom, a_label=sprintf("%.3f", error.prop)),
            data=Bayes.error,
            hjust=1)+
  a_geom_text(a_aes(text.V1.error, V2.bottom, a_label=paste(set, "Error:")),
            showSelected="neighbors",
            data=other.error,
            hjust=0)+
  a_geom_text(a_aes(text.V1.prop, V2.bottom, a_label=sprintf("%.3f", error.prop)),
            showSelected="neighbors",
            data=other.error,
            hjust=1)+
  a_geom_text(a_aes(V1, V2,
                a_label=paste0(
                  neighbors,
                  " nearest neighbor",
                  ifelse(neighbors==1, "", "s"),
                  " classifier")),
            showSelected="neighbors",
            data=show.text)
scatterPlot+
  a_facet_wrap("neighbors")+
  a_theme(panel.margin=grid::unit(0, "lines"))
viz.neighbors <- list(
  error=errorPlot,
  data=scatterPlot,
  first=list(neighbors=7),
  time=list(variable="neighbors", ms=3000)
  )
animint2dir(viz.neighbors, "knn-neighbors")

list.of.dfs <- function(...){
  name.vec <- paste(match.call()[-1])
  L <- list()
  for(name in name.vec){
    df <- get(name)
    L[[name]] <- data.frame(df)
  }
  L
}
mixtureKNN <- list.of.dfs(
  Bayes.segment,
  validation.error,
  other.error,
  show.text,
  Bayes.error,
  show.points,
  show.grid,
  pred.boundary,
  Bayes.boundary)
str(mixtureKNN)
save(mixtureKNN, file="~/R/animint/data/mixtureKNN.RData")
prompt(mixtureKNN, file="~/R/animint/man/mixtureKNN.Rd")
