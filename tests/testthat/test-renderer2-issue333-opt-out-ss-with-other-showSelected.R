acontext("opt out of showSelected legend but keep showSelected variable")
## Issue #333 https://github.com/animint/animint2/issues/333
## showSelected=character() (PR #292) turns off all legend showSelected for a layer.
## Here we want the opposite mix: keep showSelected="year" for filtering, but do not
## auto-add region from colour=region when the legend is shown. API: showSelected.legend=FALSE.
data(WorldBank, package = "animint2")
issue333_viz <- function() {
  list(
    scatter = ggplot() +
      geom_point(
        aes(fertility.rate, life.expectancy, colour = region, key = year),
        showSelected = "year",
        showSelected.legend = FALSE,
        data = WorldBank
      ),
    ts = ggplot() + make_tallrect(WorldBank, "year"),
    time = list(variable = "year", ms = 3000),
    duration = list(year = 3000),
    selector.types = list(year = "single"),
    title = "issue333 opt out legend keep showSelected"
  )
}
showSelected_variables <- function(info, plot.name = "scatter") {
  geom.names <- names(info$geoms)
  pat <- sprintf("_point_%s$", plot.name)
  point.geom.name <- geom.names[grepl(pat, geom.names)]
  stopifnot(length(point.geom.name) == 1)
  aes.map <- info$geoms[[point.geom.name[[1]]]]$aes
  ss.names <- grep("^showSelected", names(aes.map), value = TRUE)
  as.character(unlist(aes.map[ss.names]))
}
info <- animint2dir(issue333_viz(), open.browser = FALSE)
test_that("issue333 viz compiles with showSelected.legend=FALSE", {
  expect_identical(info$time$variable, "year")
})
test_that("explicit showSelected=year without legend-injected region", {
  expect_identical(showSelected_variables(info), "year")
})
test_that("colour legend for region is still compiled", {
  legend.info <- info$plots$scatter$legend$region
  expect_false(is.null(legend.info))
  expect_gt(length(legend.info$entries), 0L)
  expect_identical(legend.info$selector, "region")
})
test_that("year selector remains for animation and tallrect", {
  expect_identical(info$selectors[["year"]]$type, "single")
})
