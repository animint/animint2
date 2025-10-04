context("Guides")

test_that("colourbar trains without labels", {
  g <- guide_colorbar()
  sc <- scale_colour_continuous(limits = c(0, 4), labels = NULL)
  out <- guide_train(g, sc)
  expect_equal(names(out$key), c("colour", ".value"))
})

test_that("no error for override fill white", {
  viz <- animint(
    ggplot()+
      geom_tile(aes(
        x, x, color=y, fill=x),
        data=data.frame(x=seq(-1,1), y=0:2))+
      scale_color_gradient(
        low="white",high="black",
        guide=guide_legend(override.aes=list(fill="white")))+
      scale_fill_gradient2())
  info <- animint2dir(viz, open.browser = FALSE)
  expect_is(info, "environment")
})
