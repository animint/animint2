context("a_geom_violin")

test_that("", {
  df <- rbind(
    data.frame(x = "a", y = c(0, runif(10), 1)),
    data.frame(x = "b", y = c(0, runif(10), 2))
  )

  p <- a_plot(df, a_aes(1, y)) +
    a_geom_violin() +
    a_facet_grid(x ~ ., scales = "free") + 
    a_coord_cartesian(expand = FALSE)

  expect_equal(a_layer_scales(p, 1)$y$dimension(), c(0, 1))
  expect_equal(a_layer_scales(p, 2)$y$dimension(), c(0, 2))
})

# create_quantile_segment_frame -------------------------------------------------

test_that("create_quantile_segment_frame functions for 3 quantiles", {
  density.data <- data.frame(y=(1:256)/256, density=1/256) # uniform density

  qs <- c(0.25, 0.5, 0.75) # 3 quantiles
  expect_equal(create_quantile_segment_frame(density.data, qs)$y,
               rep(qs, each=2))
})

