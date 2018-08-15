context("Guides")

test_that("colourbar trains without labels", {
  g <- a_guide_colorbar()
  sc <- a_scale_colour_continuous(limits = c(0, 4), a_labels = NULL)

  out <- a_guide_train(g, sc)
  expect_equal(names(out$key), c("colour", ".value"))
})

