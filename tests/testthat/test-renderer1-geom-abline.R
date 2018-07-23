acontext("a_geom_abline")

p <- qplot(wt, mpg, data = mtcars) + 
  a_geom_abline(intercept = c(20, 5), slope = c(1,4)) + a_facet_wrap(~cyl)
info <- animint2HTML(list(p = p))

#Since tsv file are renamed with a_geom_abline_p_chunk1.tsv
#making similar changes in very svg @class
tsv.file <- file.path("animint-htmltest", "a_geom2_abline_p_chunk1.tsv")
tsv.data <- read.table(tsv.file, header=TRUE, comment.char = "")

test_that("columns of abline tsv", {
  expected.names <- sort(c("PANEL", "x", "xend", "y", "yend"))
  computed.names <- sort(names(tsv.data))
  expect_identical(computed.names, expected.names)
})

ablines <- getNodeSet(info$html, '//svg//g[@class="a_geom2_abline_p"]//line')
attr_ablines <- sapply(ablines, xmlAttrs)
start_ends <- attr_ablines[c("x1", "x2", "y1", "y2"), ]

test_that("All six ablines render", {
  expect_equal(length(ablines), 6)
})

test_that("Start and end of ablines are not NA", {
  expect_true(all(start_ends != "NaN"))
})

test_that("lines do not exceed ranges of plot", {
  expect_true(all(as.numeric(start_ends) >= 0))
})
