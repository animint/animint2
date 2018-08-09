acontext("aesthetics")

test_that("we stop when data does not contain interactive variables",{
  ## In interactive testing, foo will be found and copied to
  ## a_plot_build(gg)$data, but on R CMD check, animint2dir only has
  ## access to its environment, so it will not have access to foo
  ## defined in the global environment, and so calling a_plot_build on
  ## this plot from inside animint2dir will result in a "foo not found"
  ## error. However, animint should check for the validity of its
  ## interactive variables BEFORE calling a_plot_build, so below we
  ## should generate an animint error, not a a_plot_build error.
  foo <- 1 
  gg <- a_plot()+
    a_geom_point(a_aes(Sepal.Length, Petal.Length),
               showSelected="foo",
               data=iris)
  viz <- list(scatter=gg)
  expect_that({
    info <- animint2dir(viz, open.browser=FALSE)
  }, throws_error("data does not have interactive variables"))
})
