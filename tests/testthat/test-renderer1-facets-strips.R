acontext("facet-strips")

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point(colour='grey50', size = 4) + 
  geom_point(aes(colour = cyl)) 

gridViz <-
  list(gridPlot=p +
         facet_grid(cyl~am, labeller = label_both))

wrapViz <-
  list(wrapPlot=p +
         facet_wrap(~cyl+am))

test_that("facet_grid() strip labels are placed correctly", {
  info <- animint2HTML(gridViz)
  top <- getNodeSet(info$html, "//g[@class='topStrip']")
  # there should one be one 'top_strip' group (their children contain the vital info)
  kids <- xmlChildren(top[[1]])
  labs <- as.character(sapply(kids, xmlValue))
  expect_equal(labs[labs != ""], c("am: 0", "am: 1"))
  attrs <- lapply(kids, xmlAttrs)
  styles <- as.character(sapply(attrs, "[[", "style"))
  # remove leading and trailing white-space
  styles <- sub("^\\s+|\\s+$", "", styles)
  expect_equal(styles, rep("text-anchor: middle; font-size: 11pt;", 2))
  transforms <- as.character(sapply(attrs, "[[", "transform"))
  # there should one be one 'right_strip' group (their children contain the vital info)
  right <- getNodeSet(info$html, "//g[@class='rightStrip']")
  kids <- xmlChildren(right[[1]])
  labs <- as.character(sapply(kids, xmlValue))
  expect_equal(labs[labs != ""], c("cyl: 4", "cyl: 6", "cyl: 8"))
  attrs <- lapply(kids, xmlAttrs)
  styles <- as.character(sapply(attrs, "[[", "style"))
  # remove leading and trailing white-space
  styles <- sub("^\\s+|\\s+$", "", styles)
  expect_equal(styles, rep("text-anchor: middle; font-size: 11pt;", 3))
  transforms <- as.character(sapply(attrs, "[[", "transform"))
})

test_that("facet_wrap() strip labels are placed correctly", {
  info <- animint2HTML(wrapViz)
  top <- getNodeSet(info$html, "//g[@class='topStrip']")
  kids <- xmlChildren(top[[1]])
  labs <- as.character(sapply(kids, xmlValue))
  expect_equal(labs, c("4, 0", "4, 1", "6, 0", 
                       "6, 1", "8, 0", "8, 1"))
})

test_that("strip pos sensible for wrap(nrow=1)", {
  oneRow <- list(
    wrapPlot=p +
      facet_wrap(~cyl+am, nrow=1))
  info <- animint2HTML(oneRow)
  text.list <- getNodeSet(info$html, "//g[@class='topStrip']//text")
  translate.vec <- sapply(text.list, function(x)xmlAttrs(x)[["transform"]])
  translate.mat <- str_match_perl(translate.vec, translatePattern)
  trans.x <- as.numeric(translate.mat[, "x"])
  trans.y <- as.numeric(translate.mat[, "y"])
  expect_equal(diff(trans.y), rep(0, 5))
  expect_true(all(0 < diff(trans.x)))
  expect_true(all(0 < trans.x))
  expect_true(all(0 < trans.y))
})

test_that("strip pos sensible for wrap(ncol=1)", {
  oneCol <- list(
    wrapPlot=p +
      facet_wrap(~cyl+am, ncol=1))
  info <- animint2HTML(oneCol)
  text.list <- getNodeSet(info$html, "//g[@class='topStrip']//text")
  translate.vec <- sapply(text.list, function(x)xmlAttrs(x)[["transform"]])
  translate.mat <- str_match_perl(translate.vec, translatePattern)
  trans.x <- as.numeric(translate.mat[, "x"])
  trans.y <- as.numeric(translate.mat[, "y"])
  expect_true(all(0 < diff(trans.y)))
  expect_equal(diff(trans.x), rep(0, 5))
  expect_true(all(0 < trans.x))
  expect_true(all(0 < trans.y))
})

