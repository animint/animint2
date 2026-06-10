acontext("chromote timeout")

test_that("animint2HTML uses a safe page reload timeout", {
  remDr$default_timeout <- 1
  viz <- list(p=ggplot()+geom_point(aes(1, 1)))
  info <- animint2HTML(viz)
  expect_equal(remDr$default_timeout, 30)
  expect_s3_class(info$html, "XMLInternalDocument")
})
