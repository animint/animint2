acontext("compiler errors")

test_that("a_aes(showSelected=var1, showSelected2=var2) is an error", {
  viz <- list(
    petals=a_plot()+
      a_geom_point(a_aes(Petal.Width, Petal.Length,
                     showSelected=Species,
                     showSelected2=Sepal.Width),
                 data=iris)
    )
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "Use of clickSelects and showSelected as aesthetics has been deprecated. Please use as parameters")
})

test_that("informative error for time option with no showSelected", {
  viz <- list(
    petals=a_plot()+
      a_geom_point(a_aes(Petal.Width, Petal.Length),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "no interactive aes for time variable Species")
})

test_that("no error for time option with clickSelects", {
  viz <- list(
    petals=a_plot()+
      a_geom_point(a_aes(Petal.Width, Petal.Length),
                 clickSelects="Species",
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
})

test_that("no error for time option with showSelected", {
  viz <- list(
    petals=a_plot()+
      a_geom_point(a_aes(Petal.Width, Petal.Length),
                 showSelected="Species",
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
})

test_that("no error for time option with color", {
  viz <- list(
    petals=a_plot()+
      a_geom_point(a_aes(Petal.Width, Petal.Length, color=Species),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
})

data("WorldBank", package="animint2")
viz.no.duration <- list(
  scatter=a_plot()+
    a_geom_point(a_aes(x=life.expectancy, y=fertility.rate, color=region,
                   key=country),
               showSelected="year",
               clickSelects="country",
               data=WorldBank)+
    a_geom_text(a_aes(x=life.expectancy, y=fertility.rate, a_label=country),
              data=WorldBank,
              showSelected=c("year", "country", "region"),
              clickSelects="country"),
  first=list(
    year=1970,
    country=c("Canada", "India", "Pakistan", "Japan"),
    region=c("North America", "South Asia")),
  selector.types=list(country="multiple")
)

test_that("no warning for no duration vars", {
  expect_no_warning({
    info <- animint2dir(viz.no.duration, open.browser=FALSE)
  })
})

test_that("warn no key for a_geom_text with showSelected=duration var", {
  viz.duration <- viz.no.duration
  viz.duration$duration <- list(year=2000)
  expect_warning({
    info <- animint2dir(viz.duration, open.browser=FALSE)
  }, "to ensure that smooth transitions are interpretable, aes(key) should be specifed for geoms with showSelected=year, problem: a_geom2_text_scatter", fixed=TRUE)
})

viz.key.duration <- list(
  scatter=a_plot()+
    a_geom_point(a_aes(x=life.expectancy, y=fertility.rate, color=region,
                   key=country),
               showSelected="year",
               clickSelects="country",
               data=WorldBank)+
    a_geom_text(a_aes(x=life.expectancy, y=fertility.rate, a_label=country,
                  key=country),
                  showSelected=c("year", "country", "region"),
                  clickSelects="country",              
              data=WorldBank),
  first=list(
    year=1970,
    country=c("Canada", "India", "Pakistan", "Japan"),
    region=c("North America", "South Asia")),
  selector.types=list(country="multiple"),
  duration=list(year=2000)
)
test_that("no warning when key specified", {
  expect_no_warning({
    info <- animint2dir(viz.key.duration, open.browser=FALSE)
  })
})

test_that("warning for a_position=stack and showSelected", {
  set.seed(1)
  df <- data.frame(
    letter = c(replicate(4, LETTERS[1:5])),
    count = c(replicate(4, rbinom(5, 50, 0.5))),
    stack = rep(rep(1:2, each = 5), 2),
    a_facet = rep(1:2, each = 10)
    )
  df$key <- with(df, paste(stack, letter))
  gg <- a_plot() +
    a_theme_bw()+
    a_theme(panel.margin=grid::unit(0, "lines"))+
    a_geom_bar(
      a_aes(letter, count, fill = stack, key=key),
      showSelected="a_facet",
      data = df,
      a_stat = "identity",
      a_position="stack"
    )
  complicated <- list(
    plot = gg,
    time = list(variable = "a_facet", ms = 1000),
    duration = list(a_facet = 1000)
  )
  expect_warning({
    animint2dir(complicated, open.browser=FALSE)
  }, "showSelected only works with a_position=identity, problem: a_geom1_bar_plot")
})

test_that("no warning for a_position=stack without showSelected", {
  set.seed(1)
  df <- data.frame(
    letter = c(replicate(4, LETTERS[1:5])),
    count = c(replicate(4, rbinom(5, 50, 0.5))),
    stack = rep(rep(1:2, each = 5), 2),
    a_facet = rep(1:2, each = 10)
  )
  gg <- a_plot() +
    a_theme_bw()+
    a_theme(panel.margin=grid::unit(0, "lines"))+
    a_geom_bar(
      a_aes(letter, count, fill = stack),
      data = df,
      a_stat = "identity",
      a_position="stack"
    )
  no.show <- list(
    plot = gg
  )
  expect_no_warning({
    animint2dir(no.show, open.browser=FALSE)
  })
})

