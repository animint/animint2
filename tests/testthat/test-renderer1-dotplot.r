library(testthat)
library(animint2)
library(jsonlite)
test_that("JS parsing logic fails to create an array of all outliers", {
  ggplot <- animint2:::ggplot
  aes <- animint2:::aes
  geom_dotplot <- animint2:::geom_dotplot
  df <- data.frame(x = "A", y = c(1, 1.1, 1.2, 1.3, 100, 200))
  viz <- list(dotplot = ggplot() + geom_dotplot(aes(x, y), data = df))
  tmp <- file.path(tempdir(), "proof_fail")
  animint2dir(viz, out.dir = tmp, open.browser = FALSE)
  r_string <- "100 @ 200"
  broken_js_parse <- as.numeric(strsplit(r_string, " @ ")[[1]][1]) 
  expected_logic <- c(100, 200)
  expect_equal(length(broken_js_parse), length(expected_logic), 
               info = "Failure: Current JS only parses the first outlier and loses the rest!")
})
