acontext("compiler errors")

test_that("aes(showSelected=var1, showSelected2=var2) is an error", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length,
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
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "no interactive aes for time variable Species")
})

test_that("no error for time option with clickSelects", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length),
                 clickSelects="Species",
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
  expect_identical(info[["time"]][["ms"]], 3000)
})

test_that("no error for time option with showSelected", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length),
                 showSelected="Species",
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
  expect_identical(info[["time"]][["ms"]], 3000)
})

test_that("no error for time option with color", {
  viz <- list(
    petals=ggplot()+
      geom_point(aes(Petal.Width, Petal.Length, color=Species),
                 data=iris),
    time=list(variable="Species", ms=3000)
  )
  info <- animint2dir(viz, open.browser=FALSE)
  expect_identical(info[["time"]][["ms"]], 3000)
})

data("WorldBank", package="animint2")
viz.no.duration <- list(
  scatter=ggplot()+
    geom_point(aes(x=life.expectancy, y=fertility.rate, color=region,
                   key=country),
               showSelected="year",
               clickSelects="country",
               data=WorldBank)+
    geom_text(aes(x=life.expectancy, y=fertility.rate, label=country),
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

test_that("warn for -Inf but not NA input to scale_log10", {
  get_viz <- function(value){
    animint(
      g=ggplot()+
        geom_segment(aes(
          x, y, xend=xend, yend=yend),
          data=data.table(
            x=c(value, 0, 1, Inf), xend=10,
            y=10, yend=100))+
        scale_x_log10())
  }
  viz_neg_Inf <- get_viz(-Inf)
  expect_warning({
    animint2dir(viz_neg_Inf, open.browser=FALSE)
  }, "NaNs produced")
  viz_NA <- get_viz(NA)
  expect_no_warning({
    animint2dir(viz_NA, open.browser=FALSE)
  })
})

test_that("warn no key for geom_text with showSelected=duration var", {
  viz.duration <- viz.no.duration
  viz.duration$duration <- list(year=2000)
  expect_warning({
    info <- animint2dir(viz.duration, open.browser=FALSE)
  }, "to ensure that smooth transitions are interpretable, aes(key) should be specifed for geoms with showSelected=year, problem: geom2_text_scatter", fixed=TRUE)
})

viz.key.duration <- list(
  scatter=ggplot()+
    geom_point(aes(x=life.expectancy, y=fertility.rate, color=region,
                   key=country),
               showSelected="year",
               clickSelects="country",
               data=WorldBank)+
    geom_text(aes(x=life.expectancy, y=fertility.rate, label=country,
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

test_that("warning for position=stack and showSelected", {
  set.seed(1)
  df <- data.frame(
    letter = c(replicate(4, LETTERS[1:5])),
    count = c(replicate(4, rbinom(5, 50, 0.5))),
    stack = rep(rep(1:2, each = 5), 2),
    facet = rep(1:2, each = 10)
    )
  df$key <- with(df, paste(stack, letter))
  gg <- ggplot() +
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_bar(
      aes(letter, count, fill = stack, key=key),
      showSelected="facet",
      data = df,
      stat = "identity",
      position="stack"
    )
  complicated <- list(
    plot = gg,
    time = list(variable = "facet", ms = 1000),
    duration = list(facet = 1000)
  )
  expect_warning({
    animint2dir(complicated, open.browser=FALSE)
  }, "showSelected only works with position=identity, problem: geom1_bar_plot")
})

test_that("no warning for position=stack without showSelected", {
  set.seed(1)
  df <- data.frame(
    letter = c(replicate(4, LETTERS[1:5])),
    count = c(replicate(4, rbinom(5, 50, 0.5))),
    stack = rep(rep(1:2, each = 5), 2),
    facet = rep(1:2, each = 10)
  )
  gg <- ggplot() +
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_bar(
      aes(letter, count, fill = stack),
      data = df,
      stat = "identity",
      position="stack"
    )
  no.show <- list(
    plot = gg
  )
  expect_no_warning({
    animint2dir(no.show, open.browser=FALSE)
  })
})

test_that("warning for _off param without clickSelects", {
  viz.point1 <- list(
    pointone = ggplot()+
    geom_point(aes(x = wt, y = mpg),
    size = 10,
    alpha_off=0.6,
    colour_off="transparent",
    data = mtcars))
  expect_warning({
    animint2dir(viz.point1, open.browser = FALSE)
  }, "geom1_point_pointone has alpha_off, colour_off which is not used because this geom has no clickSelects; please specify clickSelects or remove alpha_off, colour_off")
})

test_that("animint(out.dir = 'dir1', out.dir = 'dir2') is an error", {
  expect_error({
    viz <- animint(out.dir = 'dir1', out.dir = 'dir2')
  }, "Duplicate named arguments are passed to animint. Duplicate argument names found: out.dir")
})

test_that("animint(plot1, plot2) is ok", {
  viz <- animint(ggplot(), ggplot())
  (computed.names <- names(viz))
  expect_identical(computed.names, c("plot1","plot2"))
})

test_that("animint(ggplot(), ggplot(), plot1=ggplot()) is ok", {
  viz <- animint(ggplot(), ggplot(), plot1=ggplot())
  (computed.names <- names(viz))
  expect_identical(computed.names, c("plot2","plot3","plot1"))
})

test_that("animint() is an error", {
  expect_error({
    animint()
  }, "No arguments passed to animint. Arguments should include ggplots(1 or more) and options(0 or more)", fixed=TRUE)
})

test_that("Same argument passed to aes and geom is an error", {
  scatter <- ggplot()+geom_path(aes(
    x=life.expectancy, 
    y=fertility.rate, 
    color=region,
    group=country, alpha=population),alpha=0.5,
    data=WorldBank)
  viz <- list(
    plot = scatter
  )
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "Same visual property cannot be defined in both aes and geom. Property defined in aes:alpha. Property defined in geom:alpha. The visual property needs only be defined in one place, so if it should be different for each rendered geom, but not depend on selection state, then it should be defined in aes; but if the property should depend on the selection state then it should be defined in geom")
})

test_that("alpha and alpha_off passed to aes and geom is an error", {
  scatter <- ggplot()+geom_path(aes(
    x=life.expectancy, 
    y=fertility.rate, 
    color=region,
    group=country, alpha=population),clickSelects="country", alpha_off=0.5,
    data=WorldBank)
  viz <- list(
    plot = scatter
  )
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "Same visual property cannot be defined in both aes and geom. Property defined in aes:alpha. Property defined in geom:alpha_off. The visual property needs only be defined in one place, so if it should be different for each rendered geom, but not depend on selection state, then it should be defined in aes; but if the property should depend on the selection state then it should be defined in geom")
})

test_that("alpha_off without clickSelects is a warning", {
  scatter <- ggplot()+geom_path(aes(
    x=life.expectancy, 
    y=fertility.rate, 
    color=region,
    group=country),alpha_off=0.5,
    data=WorldBank)
  viz <- list(
    plot = scatter
  )
  expect_warning({
    animint2dir(viz, open.browser=FALSE)
  }, "geom1_path_plot has alpha_off which is not used because this geom has no clickSelects; please specify clickSelects or remove alpha_off")
})
