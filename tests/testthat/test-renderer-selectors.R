test_that("animint renderer works without Selectors.hasOwnProperty", {
  skip_on_cran()
  
  library(animint2)
  
  df <- data.frame(
    x = 1:3,
    y = 1:3,
    grp = c("a", "b", "c")
  )
  
  p <- ggplot(df, aes(x, y)) +
    geom_point(clickSelects = "grp")
  
  tmp <- withr::local_tempdir()
  
  expect_silent(
    animint(list(plot = p), out.dir = tmp)
  )
})