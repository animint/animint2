acontext("coord")

test_that("coord_flip works", {
  data(worldPop, package="animint2")
  bars <- ggplot()+
    geom_bar(aes(x=subcontinent, y=population), showSelected="year",
             data=worldPop, stat="identity", position="identity")
  ## First test without flip.
  no.flip <- animint2dir(list(bars=bars), open.browser=FALSE)
  ax <- no.flip$plots$bars
  expect_identical(ax$xtitle, "subcontinent")
  expect_identical(ax$ytitle, "population")
  ## Then test with flip.
  flip <- animint2dir(list(bars=bars+coord_flip()), open.browser=FALSE)
  ax <- flip$plots$bars
  expect_identical(ax$ytitle, "subcontinent")
  expect_identical(ax$xtitle, "population")
})

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl))

test_that("coord_fixed with shrinking y-axis", {
  ratio5 <- 5
  viz1 <- p + coord_fixed(ratio5)
  info <- animint2HTML(list(plot = viz1))
  x.axes <- getNodeSet(info$html, "//g[contains(@class, 'xaxis')]")
  y.axes <- getNodeSet(info$html, "//g[contains(@class, 'yaxis')]")
  xdiff <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff, ydiff, ratio5)
  ## Round to drop SVG sub-pixel float noise (~1e-6); aspect should match.
  expect_equal(round(diffs[1], 3), round(diffs[2], 3))
})

test_that("xaxis width increases with coord_equal", {
  p_equal <- p + coord_equal()
  animint2HTML(animint(p_equal))
  bbox_default <- get_element_bbox("g.xaxis")
  animint2HTML(animint(p_equal+theme_animint(width=1000)))
  bbox_big <- get_element_bbox("g.xaxis")
  ratio <- bbox_big$width/bbox_default$width
  expect_gt(ratio, 2)
})

test_that("yaxis height increases with coord_fixed", {
  p_fixed <- p + coord_fixed(10)
  info_default <- animint2HTML(list(plot = p_fixed))
  y.axes <- getNodeSet(info_default$html, "//g[contains(@class, 'yaxis')]")
  ydiff_default <- getTickDiff(y.axes[[1]], axis = "y")
  info_tall <- animint2HTML(list(plot = p_fixed + theme_animint(height = 1000)))
  y.axes <- getNodeSet(info_tall$html, "//g[contains(@class, 'yaxis')]")
  ydiff_tall <- getTickDiff(y.axes[[1]], axis = "y")
  ratio <- ydiff_tall / ydiff_default
  ## Height is the filling dimension for ratio=10; tick spacing should grow
  ## substantially (width test uses expect_gt(ratio, 2) for coord_equal).
  expect_gt(ratio, 1.8)
})

test_that("coord_equal preserves aspect on non-square viewport", {
  viz <- p + coord_equal() + theme_animint(width=800, height=400)
  info <- animint2HTML(list(plot = viz))
  x.axes <- getNodeSet(info$html, "//g[contains(@class, 'xaxis')]")
  y.axes <- getNodeSet(info$html, "//g[contains(@class, 'yaxis')]")
  xdiff <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff, ydiff, 1)
  expect_equal(round(diffs[1], 3), round(diffs[2], 3))
})

test_that("coord_fixed with shrinking x-axis", {
  ratio10 <- 10
  viz2 <- p + coord_fixed(ratio10)
  info <- animint2HTML(list(plot = viz2))
  x.axes <- getNodeSet(info$html, "//g[contains(@class, 'xaxis')]")
  y.axes <- getNodeSet(info$html, "//g[contains(@class, 'yaxis')]")
  xdiff <- getTickDiff(x.axes[[1]])
  ydiff <- getTickDiff(y.axes[[1]], axis = "y")
  diffs <- normDiffs(xdiff, ydiff, ratio10)
  ## Round to drop SVG sub-pixel float noise (~1e-6); aspect should match.
  expect_equal(round(diffs[1], 3), round(diffs[2], 3))
})
