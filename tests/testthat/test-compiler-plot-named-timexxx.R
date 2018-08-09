acontext("plot named timexxx")

data(WorldBank, package = "animint2")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
not.na[not.na$country=="Kuwait", "population"] <- 1700000
viz <-
  list(scatter=a_plot()+
         a_geom_point(a_aes(life.expectancy, fertility.rate,
                        colour=region, size=population),
                    clickSelects="country",
                    showSelected="year",
                    data=not.na)+
       a_geom_text(a_aes(life.expectancy, fertility.rate, a_label=country),
                 data=not.na,
                     showSelected="country", showSelected2="year")+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),

       timeSeries=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 data=WorldBank, size=4, alpha=3/5,
                     clickSelects="country"),

       duration=list(year=1000))

test_that("plot named timeSeries is OK without time option list", {
  animint2dir(viz, open.browser=FALSE)
})

viz.time <- viz
viz.time$time <- list(ms=2000, variable="year")

test_that("plot named timeSeries is OK with time option list", {
  animint2dir(viz.time, open.browser=FALSE)
})

bad <-
  list(scatter=a_plot()+
         a_geom_point(a_aes(life.expectancy, fertility.rate,
                        colour=region, size=population),
                    clickSelects="country",
                      showSelected="year", 
                  data=not.na)+
       a_geom_text(a_aes(life.expectancy, fertility.rate, a_label=country),
                 data=not.na,
                     showSelected=c("country", "year"))+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),

       time=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 data=WorldBank, size=4, alpha=3/5,
                     clickSelects="country"),

       duration=list(year=1000))

test_that("plot named time is NOT OK", {
  expect_error({
    animint2dir(bad, open.browser=FALSE)
  }, "time option list needs ms, variable")
})

