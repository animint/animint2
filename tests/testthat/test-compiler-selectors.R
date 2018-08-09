acontext("selectors")

test_that("first options are copied to selectors", {
  data(WorldBank, package = "animint2")
  gapminder <-
    list(ts=a_plot()+
         make_tallrect(WorldBank, "year")+
         a_geom_line(a_aes(year, life.expectancy, group=country, color=region),
                   data=WorldBank, size=4, alpha=3/5,
                       clickSelects="country"),
         scatter=a_plot()+
           a_geom_point(a_aes(fertility.rate, life.expectancy,
                          colour=region, size=population),
                      clickSelects="country",
                        showSelected="year", data=WorldBank)+
         a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country),
                   data=WorldBank,
                       showSelected=c("country", "year"))+
         make_text(WorldBank, 5, 80, "year")+
         a_scale_size_animint(pixel.range=c(2,20), breaks=10^(4:9)),
         first=list(country="United States", year=1984))
  info <- animint2dir(gapminder, open.browser=FALSE)
  expect_identical(info$selectors$country$selected, "United States")
  expect_identical(info$selectors$year$selected, "1984")
})
