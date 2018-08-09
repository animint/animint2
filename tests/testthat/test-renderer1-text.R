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
viz <- list(scatter=a_plot()+
  a_geom_text(a_aes(y=fertility.rate, x=life.expectancy,
                a_label=country, size=population, colour=population),
            data=wb)+
  a_scale_size_continuous(range=c(10,20)))

test_that("text size range translates to <text font-size>", {
  info <- animint2HTML(viz)
  expect_attrs(info$html, 'text[@class="a_geom"]', "font-size", c("10", "20"))
})
              
test_that("text may contain commas and parentheses", {
  info <- animint2HTML(viz)
  a_geom <- getNodeSet(info$html, '//text[@class="a_geom"]')
  txt <- sapply(a_geom, xmlValue)
  expect_that(any(grepl("\\.", txt)), is_true())
  expect_that(any(grepl("\\(", txt)), is_true())
  expect_that(any(grepl(",", txt)), is_true())
})
