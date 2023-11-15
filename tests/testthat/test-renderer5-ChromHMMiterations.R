acontext("ChromHMMiterations data set")
library(animint2)
data(ChromHMMiterations, package = "animint2")

emission <- data.frame(ChromHMMiterations$emission, parameters="emission")
transition <- data.frame(ChromHMMiterations$transition, parameters="transition")

unique(transition$state.from)
transition$state0.to <- sprintf("%02d", transition$state.to)
emission$exp.fac <- factor(emission$experiment, unique(emission$experiment))
viz <- list(
  parameters=ggplot()+
    ggtitle("parameters at selected iteration")+
    scale_fill_gradient(low="white", high="blue")+
    scale_x_discrete("State coming from")+
    scale_y_discrete("", drop=TRUE)+
    geom_tile(aes(
      state, exp.fac, fill=frequency,
      key=paste(state, experiment)),
      showSelected="iteration",
      data=emission)+
    scale_color_gradient(low="white", high="red")+
    theme_bw()+
    theme_animint(height=500, width=350)+
    theme(panel.margin=grid::unit(0, "cm"))+
    facet_grid(
      parameters ~ .,
      space="free",
      scales="free_y")+
    geom_point(aes(
      state.from, state0.to, color=probability,
      key=paste(state.from, state.to)),
      showSelected="iteration",
      size=10,
      data=transition),
  metrics=ggplot()+
    ggtitle("convergence metrics, select iteration")+
    make_tallrect(ChromHMMiterations$metrics, "iteration")+
    geom_line(aes(
      iteration, metric.value),
      data=ChromHMMiterations$metrics)+
    theme_bw()+
    theme_animint(height=500)+
    facet_grid(metric.name ~ ., scales="free_y"),
  duration=list(iteration=500),
  first=list(iteration=100),
  title="ChromHMM parameter fitting for one iPS sample")
viz$param

expect_no_warning({
  info <- animint2HTML(viz)
})

test_that("no vertical space between border_rects", {
  rect.list <- getNodeSet(
    info$html, '//svg[@id="plot_parameters"]//rect[@class="border_rect"]')
  expect_equal(length(rect.list), 2)
  first <- xmlAttrs(rect.list[[1]])
  first.bottom <- as.numeric(first[["y"]])+as.numeric(first[["height"]])
  second <- xmlAttrs(rect.list[[2]])
  second.top <- as.numeric(second[["y"]])
  expect_equal(first.bottom, second.top)
})

test_that("fill not constant in probability legend and circles", {
  fill.vec <- getStyleValue(
    info$html, '//svg[@id="plot_parameters"]//circle', "fill")
  expect_true(1 < length(table(fill.vec)))
  fill.vec <- getStyleValue(
    info$html, '//tr[@class="probability_variable"]//circle', "fill")
  expect_true(1 < length(table(fill.vec)))
})

test_that("tile stroke is black", {
  stroke.vec <- getStyleValue(
    info$html, '//g[@class="geom1_tile_parameters"]//rect', "stroke")
  expect_color(stroke.vec, "black")
})  
