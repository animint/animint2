acontext("JSON migration validation for issue #193")
## This test validates jsonlite compatibility for RJSONIO migration.
## See https://github.com/animint/animint2/issues/193
## RJSONIO is no longer maintained on CRAN, so we migrate to jsonlite.
## The key requirement: auto_unbox=TRUE makes jsonlite match RJSONIO behavior.
test_that("jsonlite encodes single values without array wrapping when auto_unbox=TRUE", {
  data <- list(geom="point", size=3, enabled=TRUE)
  json_str <- jsonlite::toJSON(data, auto_unbox=TRUE)
  expect_false(grepl('\\["point"\\]', json_str))
  expect_false(grepl('\\[3\\]', json_str))
  expect_true(grepl('"geom"\\s*:\\s*"point"', json_str))
  expect_true(grepl('"size"\\s*:\\s*3', json_str))
})
test_that("jsonlite preserves vector arrays correctly", {
  data <- list(x=c(1,2,3), labels=c("a","b","c"))
  json_str <- jsonlite::toJSON(data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(length(parsed$x), 3)
  expect_equal(length(parsed$labels), 3)
  expect_equal(parsed$x[[1]], 1)
  expect_equal(parsed$labels[[2]], "b")
})
test_that("jsonlite handles nested list structures like plot.json geoms", {
  geom_data <- list(
    geom1_point_plot=list(
      geom="point",
      classed="geom1_point_plot",
      aes=list(x="x", y="y"),
      params=list(na.rm=FALSE, size=3),
      chunks=1,
      total=1
    )
  )
  json_str <- jsonlite::toJSON(geom_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(parsed$geom1_point_plot$geom, "point")
  expect_equal(parsed$geom1_point_plot$aes$x, "x")
  expect_equal(parsed$geom1_point_plot$params$size, 3)
  expect_equal(parsed$geom1_point_plot$chunks, 1)
})
test_that("jsonlite handles plot layout data with boolean arrays", {
  layout_data <- list(
    PANEL=c("1","2","3","4"),
    ROW=c(1,1,2,2),
    COL=c(1,2,1,2),
    AXIS_X=c(FALSE,FALSE,TRUE,TRUE),
    AXIS_Y=c(TRUE,FALSE,TRUE,FALSE)
  )
  json_str <- jsonlite::toJSON(layout_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(length(parsed$PANEL), 4)
  expect_equal(parsed$AXIS_X[[3]], TRUE)
  expect_equal(parsed$AXIS_Y[[2]], FALSE)
})
test_that("jsonlite handles axis tick data arrays", {
  axis_data <- list(
    x=c(2.5, 5, 7.5, 10),
    xlab=c("2.5", "5.0", "7.5", "10.0"),
    xrange=c(0.55, 10.45),
    xline=TRUE,
    xticks=TRUE
  )
  json_str <- jsonlite::toJSON(axis_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(length(parsed$x), 4)
  expect_equal(parsed$xrange[[1]], 0.55)
  expect_equal(parsed$xline, TRUE)
})
test_that("jsonlite handles nested grid location arrays", {
  grid_data <- list(
    grid_major=list(
      colour="#FFFFFF",
      size=0.5,
      loc=list(
        x=list(c(2.5,5,7.5,10), c(2.5,5,7.5,10)),
        y=list(c(2.5,5,7.5,10), c(2.5,5,7.5,10))
      )
    )
  )
  json_str <- jsonlite::toJSON(grid_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(parsed$grid_major$colour, "#FFFFFF")
  expect_equal(length(parsed$grid_major$loc$x), 2)
  expect_equal(length(parsed$grid_major$loc$x[[1]]), 4)
})
test_that("jsonlite handles selector structures for interactivity", {
  selector_data <- list(
    selectors=list(
      year=list(selected="2000", type="single")
    ),
    first=list(year="2000"),
    time=list(ms=2000, variable="year"),
    duration=list(year=500)
  )
  json_str <- jsonlite::toJSON(selector_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(parsed$selectors$year$selected, "2000")
  expect_equal(parsed$time$ms, 2000)
  expect_equal(parsed$duration$year, 500)
})
test_that("jsonlite handles empty lists and structures", {
  data <- list(legend=list(), panel_border=list(), selectors=list())
  json_str <- jsonlite::toJSON(data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_true("legend" %in% names(parsed))
  expect_true("panel_border" %in% names(parsed))
  expect_equal(length(parsed$legend), 0)
})
test_that("jsonlite handles strip and facet data", {
  strip_data <- list(
    strips=list(
      top=c("A","B","C","D"),
      right=c(""),
      n=list(top=2, right=0)
    )
  )
  json_str <- jsonlite::toJSON(strip_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(length(parsed$strips$top), 4)
  expect_equal(parsed$strips$n$top, 2)
})
test_that("jsonlite handles panel styling parameters", {
  style_data <- list(
    panel_background=list(
      fill="#EBEBEB",
      colour="transparent",
      size=0.5,
      linetype=1
    )
  )
  json_str <- jsonlite::toJSON(style_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(parsed$panel_background$fill, "#EBEBEB")
  expect_equal(parsed$panel_background$size, 0.5)
})
test_that("jsonlite handles chunk_info metadata", {
  chunk_data <- list(
    chunk_info=list(
      "geom1_point_chunk1.tsv"=list(bytes=258, rows=40)
    )
  )
  json_str <- jsonlite::toJSON(chunk_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  tsv_info <- parsed$chunk_info[["geom1_point_chunk1.tsv"]]
  expect_equal(tsv_info$bytes, 258)
  expect_equal(tsv_info$rows, 40)
})
test_that("jsonlite round-trip preserves complete export.data structure", {
  ## Mimics the export.data structure from R/z_animint.R line 631-639
  export_data <- list(
    geoms=list(
      geom1_point_scatter=list(
        geom="point",
        classed="geom1_point_scatter",
        aes=list(x="x", y="y"),
        params=list(na.rm=FALSE, size=3),
        types=list(x="numeric", y="numeric"),
        chunk_order=list(),
        nest_order=c("PANEL"),
        subset_order=c("PANEL"),
        chunks=1,
        total=1
      )
    ),
    time=list(ms=2000, variable="year"),
    duration=list(year=500),
    selectors=list(year=list(selected="2000", type="single")),
    plots=list(
      scatter=list(
        panel_margin_lines=0.25,
        legend=list(),
        xtitle="X Axis",
        ytitle="Y Axis",
        title="Test Plot",
        options=list(width=600, height=400),
        geoms=c("geom1_point_scatter")
      )
    )
  )
  json_str <- jsonlite::toJSON(export_data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(parsed$geoms$geom1_point_scatter$geom, "point")
  expect_equal(parsed$plots$scatter$title, "Test Plot")
  expect_equal(parsed$time$ms, 2000)
  expect_equal(parsed$selectors$year$selected, "2000")
  expect_equal(parsed$plots$scatter$options$width, 600)
})
test_that("jsonlite output is valid JSON parseable by JavaScript", {
  ## This test ensures the JSON string format is valid
  data <- list(
    plot="scatter",
    data=list(x=c(1,2,3), y=c(4,5,6)),
    mapping=list(x="x", y="y")
  )
  json_str <- jsonlite::toJSON(data, auto_unbox=TRUE)
  ## Valid JSON should start with { and end with }
  expect_true(grepl("^\\s*\\{", json_str))
  expect_true(grepl("\\}\\s*$", json_str))
  ## Should not have R-specific artifacts
  expect_false(grepl("NA", json_str))
  expect_false(grepl("NULL", json_str))
})
test_that("jsonlite handles numeric precision for axis ranges", {
  data <- list(xrange=c(0.55, 10.45), yrange=c(-3.14159, 2.71828))
  json_str <- jsonlite::toJSON(data, auto_unbox=TRUE)
  parsed <- jsonlite::fromJSON(json_str, simplifyVector=FALSE)
  expect_equal(parsed$xrange[[1]], 0.55, tolerance=1e-10)
  expect_equal(parsed$yrange[[1]], -3.14159, tolerance=1e-5)
})
test_that("single-panel layout columns remain JSON arrays in plot.json (#193)", {
  tmp <- tempfile()
  p <- ggplot(data.frame(x=1:3, y=1:3), aes(x, y)) + geom_point()
  animint2dir(list(p=p), out.dir=tmp, open.browser=FALSE)
  j <- jsonlite::fromJSON(file.path(tmp, "plot.json"), simplifyVector=FALSE)
  expect_type(j$plots$p$layout$PANEL, "list")
  expect_length(j$plots$p$layout$PANEL, 1)
  expect_type(j$plots$p$layout$ROW, "list")
  expect_type(j$plots$p$layout$COL, "list")
  expect_type(j$plots$p$layout$AXIS_X, "list")
  expect_type(j$plots$p$layout$AXIS_Y, "list")
})
