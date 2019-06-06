library(testthat)
acontext("compiler print")

viz <- animint(
  foo=ggplot()+
    geom_point(aes(
      Petal.Length, Sepal.Length, color=Species),
      data=iris))

test_that("message when out.dir unspecified", {
  expect_message({
    print(viz)
  }, 'Saving animint in temporary directory; specify output directory using animint(out.dir="path/to/directory")', fixed=TRUE)
})

out.dir <- file.path(tempdir(), "animint-out")
unlink(out.dir, recursive=TRUE)
vizout <- viz
vizout$out.dir <- out.dir
test_that("out.dir option is respected", {
  expect_false(file.exists(out.dir))
  print(vizout)
  expect_true(file.exists(out.dir))
})
