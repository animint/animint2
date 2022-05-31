acontext("Text")

data(WorldBank, package = "animint2")
wb2010 <- subset(WorldBank, year==2010)
subset(wb2010, population==min(population))
### This test does not pass if we use the wb2010 data set above rather
### than the wb data set below. The problem is that the scales are
### trained before NAs are removed from the data. There are NA values
### in life.expectancy/fertility.rate for Tuvalu, but not in
### population. So Tuvalu will not be rendered on the plot, and in
### fact there will be no text element with fontsize=10!
wb <- subset(wb2010, !is.na(population) &
    !is.na(fertility.rate) & !is.na(life.expectancy))
viz <- list(scatter=ggplot()+
  geom_text(aes(y=fertility.rate, x=life.expectancy,
                label=country, size=population, colour=population),
            data=wb)+
  scale_size_continuous(range=c(10,20)))

test_that("text size range translates to <text font-size>", {
  info <- animint2HTML(viz)
  expect_attrs(info$html, 'text[@class="geom"]', "font-size", c("10", "20"))
})
              
test_that("text may contain commas and parentheses", {
  info <- animint2HTML(viz)
  geom <- getNodeSet(info$html, '//text[@class="geom"]')
  txt <- sapply(geom, xmlValue)
  expect_true(any(grepl("\\.", txt)))
  expect_true(any(grepl("\\(", txt)))
  expect_true(any(grepl(",", txt)))
})

### Test text rotation option
labs <- sapply(0:8, function (x) paste("rot", x, sep=""))
angle <- sapply(0:8, function(x) x * 45)
plot.vec <- data.frame(
  x = 0:8,
  y = 0:8,
  labs,
  angle
)

viz.aes.angle <- list(scatter = scatter.plot <- ggplot() +
  geom_text(
    data=plot.vec,
    aes(x = x, y = y, label = labs, angle = angle),
    clickSelects = "x",
    size = 30
  ))

test_that("text rotation applies to <text transform> when applied in aes", {
  info <- animint2HTML(viz.aes.angle)
  # can't use expect_attrs because the exact x and y coordinates in the
  # transform are not known to in advance
  geom <- getNodeSet(info$html, '//text[@class="geom"]')
  transform <- data.frame(t(sapply(geom, xmlAttrs)))$transform
  expect_true(any(grepl("-45", transform)))
  expect_true(any(grepl("-90", transform)))
  expect_true(any(grepl("0", transform)))
})

viz.geom.angle <- list(scatter = scatter.plot <- ggplot() +
  geom_text(
    data = plot.vec,
    aes(x = x, y = y, label = labs),
    angle = 90,
    clickSelects = "x",
    size = 30
  ))


test_that("text rotation applies to <text transform> when used in geom", {
  info <- animint2HTML(viz.geom.angle)
  # can't use expect_attrs because the exact x and y coordinates in the
  # transform are not known to in advance
  geom <- getNodeSet(info$html, '//text[@class="geom"]')
  transform <- data.frame(t(sapply(geom, xmlAttrs)))$transform
  expect_true(any(grepl("-90", transform)))
})