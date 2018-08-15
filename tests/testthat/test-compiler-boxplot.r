context("Boxplot")

# thanks wch for providing the test code
test_that("a_geom_boxplot range includes all outliers", {
  dat <- data.frame(x = 1, y = c(-(1:20) ^ 3, (1:20) ^ 3) )
  p <- a_plot_build(a_plot(dat, a_aes(x,y)) + a_geom_boxplot())

  miny <- p$panel$ranges[[1]]$y.range[1]
  maxy <- p$panel$ranges[[1]]$y.range[2]

  expect_true(miny <= min(dat$y))
  expect_true(maxy >= max(dat$y))
})

test_that("a_geom_boxplot for continuous x gives warning if more than one x (#992)", {
  dat <- expand.grid(x = 1:2, y = c(-(1:5) ^ 3, (1:5) ^ 3) )

  bplot <- function(a_aes = NULL, extra = list()) {
    a_plot_build(a_plot(dat, a_aes) + a_geom_boxplot(a_aes) + extra)
  }

  expect_warning(bplot(a_aes(x, y)), "Continuous x aesthetic")
  expect_warning(bplot(a_aes(x, y), a_facet_wrap(~x)), "Continuous x aesthetic")
  expect_warning(bplot(a_aes(Sys.Date() + x, y)), "Continuous x aesthetic")

  expect_warning(bplot(a_aes(x, group = x, y)), NA)
  expect_warning(bplot(a_aes(1, y)), NA)
  expect_warning(bplot(a_aes(factor(x), y)), NA)
  expect_warning(bplot(a_aes(x == 1, y)), NA)
  expect_warning(bplot(a_aes(as.character(x), y)), NA)
})
