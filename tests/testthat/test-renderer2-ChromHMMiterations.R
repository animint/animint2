acontext("ChromHMMiterations data set")

data(ChromHMMiterations, package = "animint2")

emission <- data.frame(ChromHMMiterations$emission, parameters="emission")
transition <- data.frame(ChromHMMiterations$transition, parameters="transition")

viz <- list(
  parameters=a_plot()+
    ggtitle("parameters at selected iteration")+
    a_scale_fill_gradient(low="white", high="blue")+
    a_geom_tile(a_aes(state, experiment, fill=frequency,
                  key=paste(state, experiment)),
              showSelected="iteration",
              data=emission)+
    a_scale_color_gradient(low="white", high="red")+
    a_theme_bw()+
    a_theme_animint(height=600, width=350)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_facet_grid(parameters ~ .,
               space="free",
               scales="free_y")+
    a_scale_y_discrete(drop=FALSE)+
    a_geom_point(a_aes(state.to, state.from, color=probability,
                  key=paste(state.from, state.to)),
               showSelected="iteration",
               size=10,
               data=transition),
  metrics=a_plot()+
    ggtitle("convergence metrics, select iteration")+
    make_tallrect(ChromHMMiterations$metrics, "iteration")+
    a_geom_line(a_aes(iteration, metric.value),
              data=ChromHMMiterations$metrics)+
    a_theme_bw()+
    a_theme_animint(height=500)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_facet_grid(metric.name ~ ., scales="free_y"),
  duration=list(iteration=500),
  first=list(iteration=100),
  title="ChromHMM parameter fitting for one iPS sample")

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
