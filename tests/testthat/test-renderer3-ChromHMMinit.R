acontext("ChromHMMinit data set")

require(httr)
ChromHMMinit.RData <- file.path(tempdir(), "ChromHMMinit.RData")
request <- GET("http://github.com/tdhock/animint-examples/blob/master/data/ChromHMMinit.RData?raw=true")
stop_for_status(request)
writeBin(content(request), ChromHMMinit.RData)
## If we don't load this data set into the global environment, then we
## get Error in eval(expr, envir, enclos) (from helper-functions.R#5)
## : object 'ChromHMMinit' not found
load(ChromHMMinit.RData, .GlobalEnv) 

last.iteration <- subset(ChromHMMinit$metrics, iteration==100)

viz <- list(
  parameters=a_plot()+
    ggtitle("parameters at selected iteration")+
    a_scale_fill_gradient(low="white", high="blue")+
    a_geom_tile(a_aes(state, experiment, fill=frequency,
                  key=paste(state, experiment)),
              showSelected=c("repeat.fac", "iteration"),
              ##chunk_vars=c("repeat.fac"),
              data=data.frame(ChromHMMinit$emission, parameters="emission"))+
    a_scale_color_gradient(low="white", high="red")+
    a_theme_bw()+
    a_theme_animint(height=500, width=400)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_facet_grid(parameters ~ .,
               space="free",
               scales="free_y")+
    a_scale_y_discrete(drop=FALSE)+
    a_geom_point(a_aes(state.to, state.from, color=probability,
                   key=paste(state.from, state.to)),
               showSelected=c("repeat.fac", "iteration"),
               size=8,
               ##chunk_vars=c("repeat.fac"),
               data=data.frame(ChromHMMinit$transition,
                 parameters="transition")),
  metrics=a_plot()+
    ggtitle("convergence metrics, select iteration")+
    make_tallrect(ChromHMMinit$metrics, "iteration")+
    a_geom_line(a_aes(iteration, metric.value,
                  group=repeat.fac),
              clickSelects="repeat.fac",
              size=3,
              alpha=0.6,
              data=subset(ChromHMMinit$metrics, metric.name != "Change"))+
    a_theme_bw()+
    a_theme_animint(height=500)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_facet_grid(metric.name ~ ., scales="free_y"),
  last=a_plot()+
    ggtitle("last iteration, select initialization")+
    a_theme_bw()+
    a_theme_animint(height=500, width=400)+
    a_theme(panel.margin=grid::unit(0, "cm"))+
    a_facet_grid(metric.name ~ ., space="fixed", scales="free")+
    a_geom_point(a_aes(repeat.fac, metric.value),
               clickSelects="repeat.fac",
               size=5,
               data=subset(last.iteration, metric.name != "Change"))+
    a_scale_x_discrete("random initialization")+
    a_scale_y_continuous(""),
  duration=list(iteration=500),
  first=list(iteration=100),
  time=list(variable="iteration", ms=500),
  title="10 ChromHMM fits for 6 experiments on one iPS sample")

system.time({
  info <- animint2HTML(viz) #TODO: why does this take so long?
})

getFill <- function(html=getHTML()){
  getStyleValue(html, '//g[@class="a_geom2_point_parameters"]//circle', "fill")
}

test_that("animation starts by default", {
  initial.fill.vec <- getFill()
  expect_equal(length(initial.fill.vec), 225)
  Sys.sleep(5)
  updated.fill.vec <- getFill()
  expect_equal(length(updated.fill.vec), 225)
  n.different <- sum(initial.fill.vec != updated.fill.vec)
  expect_more_than(n.different, 0)
})
