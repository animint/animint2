test_that("geom_point warns for shape != 21", {
  expect_warning(
    animint(p = ggplot() + geom_point(aes(Sepal.Length, Sepal.Width), data=iris, shape=1)),
    "shape.*21"
  )
  
  expect_warning(
    animint(p = ggplot() + geom_point(aes(Sepal.Length, Sepal.Width, shape=Species), data=iris)),
    "shape.*21"
  )
  
  expect_silent(
    animint(p = ggplot() + geom_point(aes(Sepal.Length, Sepal.Width), data=iris, shape=21))
  )
})
