context("Data")

test_that("stringsAsFactors doesn't affect results", {
  sAF <- getOption("stringsAsFactors")
  dat.character <- data.frame(x = letters[5:1], y = 1:5, stringsAsFactors = FALSE)
  dat.factor <- data.frame(x = letters[5:1], y = 1:5, stringsAsFactors = TRUE)
  base <- ggplot(mapping = aes(x, y)) + geom_point()
  xlabels <- function(x) x$panel$ranges[[1]]$x.labels
  suppressWarnings(options(stringsAsFactors = TRUE))
  char_true <- ggplot_build(base %+% dat.character)
  factor_true <- ggplot_build(base %+% dat.factor)
  options(stringsAsFactors = FALSE)
  char_false <- ggplot_build(base %+% dat.character)
  factor_false <- ggplot_build(base %+% dat.factor)
  options(stringsAsFactors = sAF)
  expect_equal(xlabels(char_true), letters[1:5])
  expect_equal(xlabels(char_false), letters[1:5])
  expect_equal(xlabels(factor_true), letters[1:5])
  expect_equal(xlabels(factor_false), letters[1:5])
})

test_that("animint works with data.table", {
  year.int <- as.integer(floor(time(co2)))
  month.int <- as.integer(time(co2) %% 1 * 12 + 1)
  year.month.chr <- sprintf("%d-%02d-15", year.int, month.int)
  year.month.POSIXct <- as.POSIXct(strptime(year.month.chr, "%Y-%m-%d"))
  month.str <- strftime(year.month.POSIXct, "%B")
  month <- factor(month.str, month.str[1:12])
  library(data.table)
  co2.details <- data.table(
    year.int,
    month.int,
    month,
    year.month.POSIXct,
    ppm=as.numeric(co2))
  TimeSeries <- list(
    ts = ggplot()+
      geom_line(aes(year.month.POSIXct, ppm),
                size=1,
                data=co2.details)+
      geom_point(aes(year.month.POSIXct, ppm, color=month),
                 data=co2.details)+
      xlab("Time of measurement")+
      ylab("Atmospheric carbon dioxide (parts per million)"))
  expect_no_warning({
    info <- animint2dir(TimeSeries, open.browser=FALSE)
  })
  expect_is(co2.details, "data.table")
})
