acontext("mixtureKNN data set")

data(mixtureKNN)

mixtureKNN$Bayes.error$text.V1.prop <- 0
mixtureKNN$Bayes.error$text.V2.bottom <- -2
mixtureKNN$other.error$text.V1.prop <- 0
mixtureKNN$Bayes.error$text.V1.error <- -2.6
mixtureKNN$other.error$text.V1.error <- -2.6
classifier.linetypes <- c(
  Bayes="dashed",
  KNN="solid")
label.colors <- c(
  "0"="#377EB8",
  "1"="#FF7F00")
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
            data=mixtureKNN$Bayes.segment)+
  a_geom_segment(a_aes(min.neighbors, error.prop, 
                   xend=max.neighbors, yend=error.prop,
                   color=set, linetype=classifier),
               showSelected="classifier",
               data=mixtureKNN$Bayes.segment)+
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
              showSelected=c("classifier","set"),
              alpha=0.5,
              data=mixtureKNN$validation.error)+
  a_geom_line(a_aes(neighbors, mean, color=set, linetype=classifier),
            showSelected="classifier",
            data=mixtureKNN$validation.error)+
  a_geom_line(a_aes(neighbors, error.prop, group=set, color=set,
                linetype=classifier),
            showSelected="classifier",
            data=mixtureKNN$other.error)+
  a_geom_tallrect(a_aes(xmin=neighbors-1, xmax=neighbors+1),
                clickSelects="neighbors",
                alpha=0.5,
                data=mixtureKNN$validation.error)
errorPlot

scatterPlot <- a_plot()+
  ggtitle("Mis-classification errors in train set")+
  a_theme_bw()+
  a_theme_animint(width=500, height=500)+
  xlab("Input feature 1")+
  ylab("Input feature 2")+
  a_coord_equal()+
  a_scale_color_manual(values=label.colors)+
  a_scale_linetype_manual(values=classifier.linetypes)+
  a_geom_point(a_aes(V1, V2, color=label),
             showSelected="neighbors",
             size=0.2,
             data=mixtureKNN$show.grid)+
  a_geom_path(a_aes(V1, V2, group=path.i, linetype=classifier),
            showSelected="neighbors",
            size=1,
            data=mixtureKNN$pred.boundary)+
  a_geom_path(a_aes(V1, V2, group=path.i, linetype=classifier),
            color=set.colors[["test"]],
            size=1,
            data=mixtureKNN$Bayes.boundary)+
  a_geom_point(a_aes(V1, V2, color=label,
                 fill=prediction),
             showSelected="neighbors",
             size=3,
             shape=21,
             data=mixtureKNN$show.points)+
  a_scale_fill_manual(values=c(error="black", correct="transparent"))+
  a_geom_text(a_aes(text.V1.error, text.V2.bottom, a_label=paste(set, "Error:")),
            data=mixtureKNN$Bayes.error,
            hjust=0)+
  a_geom_text(a_aes(text.V1.prop, text.V2.bottom, a_label=sprintf("%.3f", error.prop)),
            data=mixtureKNN$Bayes.error,
            hjust=1)+
  a_geom_text(a_aes(text.V1.error, V2.bottom, a_label=paste(set, "Error:")),
            showSelected="neighbors",
            data=mixtureKNN$other.error,
            hjust=0)+
  a_geom_text(a_aes(text.V1.prop, V2.bottom, a_label=sprintf("%.3f", error.prop)),
            showSelected="neighbors",
            data=mixtureKNN$other.error,
            hjust=1)+
  a_geom_text(a_aes(V1, V2,
                a_label=paste0(
                  neighbors,
                  " nearest neighbor",
                  ifelse(neighbors==1, "", "s"),
                  " classifier")),
            showSelected="neighbors",
            data=mixtureKNN$show.text)
scatterPlot+
  a_facet_wrap("neighbors")+
  a_theme(panel.margin=grid::unit(0, "lines"))

viz.neighbors <- list(
  error=errorPlot,
  data=scatterPlot,
  first=list(neighbors=7)
  )
info <- animint2HTML(viz.neighbors)

get_nodes <- function(html=getHTML()){
  line.list <- getNodeSet(html, "//g[@class='a_geom2_segment_error']//line")
  rect.list <- getNodeSet(
    html, "//svg[@id='plot_error']//rect[@class='border_rect']")
  rect.attr.mat <- sapply(rect.list, xmlAttrs)
  rect.x <- as.numeric(rect.attr.mat["x",])
  rect.width <- as.numeric(rect.attr.mat["width",])
  rect.right <- rect.x + rect.width
  line.attr.mat <- sapply(line.list, xmlAttrs)
  list(
    ribbon=getNodeSet(html, "//g[@class='a_geom3_ribbon_error']//path"),
    validation=getNodeSet(html, "//g[@class='a_geom4_line_error']//path"),
    train.test=getNodeSet(html, "//g[@class='a_geom5_line_error']//path"),
    Bayes=line.list,
    Bayes.x2=if(is.matrix(line.attr.mat))as.numeric(line.attr.mat["x2",]),
    border.right=rect.right,
    boundary.KNN=getNodeSet(html, "//g[@class='a_geom8_path_data']//path"),
    boundary.Bayes=getNodeSet(html, "//g[@class='a_geom9_path_data']//path")
    )
}

before <- get_nodes(info$html)
test_that("1 <path> rendered for validation error band", {
  expect_equal(length(before$ribbon), 1)
})
test_that("1 <path> rendered for validation error mean", {
  expect_equal(length(before$validation), 1)
})
test_that("2 <path> rendered for train/test error", {
  expect_equal(length(before$train.test), 2)
})
test_that("1 <line> rendered for Bayes error", {
  expect_equal(length(before$Bayes), 1)
})
test_that("Bayes error <line> inside of border_rect", {
  expect_less_than(before$Bayes.x2, before$border.right)
})
test_that("6 <path> rendered for KNN boundary", {
  expect_equal(length(before$boundary.KNN), 6)
})
test_that("2 <path> rendered for Bayes boundary", {
  expect_equal(length(before$boundary.Bayes), 2)
})

clickID("plot_data_classifier_variable_Bayes")

click1 <- get_nodes()
test_that("first click, 1 <path> rendered for validation error band", {
  expect_equal(length(click1$ribbon), 1)
})
test_that("first click, 1 <path> rendered for validation error mean", {
  expect_equal(length(click1$validation), 1)
})
test_that("first click, 2 <path> rendered for train/test error", {
  expect_equal(length(click1$train.test), 2)
})
test_that("first click, Bayes error disappears", {
  expect_equal(length(click1$Bayes), 0)
})
test_that("first click, 6 <path> rendered for KNN boundary", {
  expect_equal(length(click1$boundary.KNN), 6)
})
test_that("first click, Bayes boundary disappears", {
  expect_equal(length(click1$boundary.Bayes), 0)
})

clickID("plot_data_classifier_variable_KNN")

click2 <- get_nodes()
test_that("second click, validation error band disappears", {
  expect_equal(length(click2$ribbon), 0)
})
test_that("second click, validation error mean disappears", {
  expect_equal(length(click2$validation), 0)
})
test_that("second click, train/test error disappears", {
  expect_equal(length(click2$train.test), 0)
})
test_that("second click, Bayes error still gone", {
  expect_equal(length(click2$Bayes), 0)
})
test_that("second click, KNN boundary disappears", {
  expect_equal(length(click2$boundary.KNN), 0)
})
test_that("second click, Bayes boundary still gone", {
  expect_equal(length(click2$boundary.Bayes), 0)
})
