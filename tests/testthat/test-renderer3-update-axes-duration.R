acontext("update_axes respects selector duration - Issue #276")
data(WorldBank, package="animint2")
# Create test data with clear axis range differences
WorldBank$year.fac <- factor(WorldBank$year)
viz_custom_duration <- list(
  scatter = ggplot() +
    geom_point(aes(life.expectancy, fertility.rate, color=year.fac),
               showSelected="year.fac",
               data=WorldBank) +
    theme_animint(update_axes=c("x", "y")),
  ts = ggplot() +
    geom_tallrect(aes(xmin=as.numeric(year.fac)-0.5, xmax=as.numeric(year.fac)+0.5),
                  clickSelects="year.fac",
                  alpha=0.5,
                  data=WorldBank),
  duration=list(year.fac=2000),
  first=list(year.fac="1975"),
  selector.types=list(year.fac="single")
)
info <- animint2HTML(viz_custom_duration)
get_axis_transform <- function(html, plot_id, axis_class){
  xpath <- sprintf('//svg[@id="plot_%s"]//g[@class="%s axis %s_1"]', plot_id, axis_class, axis_class)
  nodes <- getNodeSet(html, xpath)
  if(length(nodes) == 0) return(NULL)
  xmlGetAttr(nodes[[1]], "transform")
}
html_before <- getHTML()
xaxis_before <- get_axis_transform(html_before, "scatter", "xaxis")
yaxis_before <- get_axis_transform(html_before, "scatter", "yaxis")
clickID("plot_ts_year.fac_variable_1980")
Sys.sleep(1.2)
html_at_1200ms <- getHTML()
xaxis_at_1200ms <- get_axis_transform(html_at_1200ms, "scatter", "xaxis")
yaxis_at_1200ms <- get_axis_transform(html_at_1200ms, "scatter", "yaxis")
Sys.sleep(1.0)
html_after <- getHTML()
xaxis_after <- get_axis_transform(html_after, "scatter", "xaxis")
yaxis_after <- get_axis_transform(html_after, "scatter", "yaxis")
test_that("axis transitions respect custom 2000ms duration not hardcoded 1000ms", {
  expect_true(!is.null(xaxis_before), info="X-axis should exist before click")
  expect_true(!is.null(yaxis_before), info="Y-axis should exist before click")
  expect_true(!is.null(xaxis_at_1200ms), info="X-axis should exist at 1200ms")
  expect_true(!is.null(yaxis_at_1200ms), info="Y-axis should exist at 1200ms")
  expect_true(!is.null(xaxis_after), info="X-axis should exist after transition")
  expect_true(!is.null(yaxis_after), info="Y-axis should exist after transition")
  expect_true(xaxis_before != xaxis_after, info=sprintf("X-axis should change from year 1975 to 1980. Before: %s, After: %s", xaxis_before, xaxis_after))
  expect_true(yaxis_before != yaxis_after, info=sprintf("Y-axis should change from year 1975 to 1980. Before: %s, After: %s", yaxis_before, yaxis_after))
  expect_true(xaxis_at_1200ms != xaxis_after, info=sprintf("With 2000ms duration, X-axis should still be transitioning at 1200ms (60%% complete). At 1200ms: %s, Final: %s. If hardcoded to 1000ms, transition would be complete and these would be equal.", xaxis_at_1200ms, xaxis_after))
  expect_true(yaxis_at_1200ms != yaxis_after, info=sprintf("With 2000ms duration, Y-axis should still be transitioning at 1200ms (60%% complete). At 1200ms: %s, Final: %s. If hardcoded to 1000ms, transition would be complete and these would be equal.", yaxis_at_1200ms, yaxis_after))
})
