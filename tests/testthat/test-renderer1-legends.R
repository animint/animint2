acontext("legends")

data(WorldBank, package="animint2")
breaks <- 10^(4:9)
viz <-
  list(ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=WorldBank, size=3, alpha=3/5),
       scatter=a_plot()+
       a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population),
                  clickSelects="country",
                  showSelected=c("year"),
                  data=WorldBank)+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country),
                 showSelected=c("country", "year"),
                 data=WorldBank)+
       make_text(WorldBank, 5, 80, "year")+
       a_scale_size_animint(breaks=breaks))

test_that('breaks are respected', {
  info <- animint2dir(viz, open.browser=FALSE)
  entries <- info$plots$scatter$legend$population$entries
  a_label.chr <- sapply(entries, "[[", "a_label")
  a_label.num <- as.numeric(a_label.chr)
  expect_equal(sort(a_label.num), sort(breaks))
})

test_that('hiding both legends works with a_geom_point(show.legend=FALSE)', {
  viz$scatter <- a_plot()+
    a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population),
               clickSelects="country",
               showSelected=c("year"),
               data=WorldBank, show.legend=FALSE)+
    a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country),
              showSelected=c("country", "year"),
              data=WorldBank)+
    make_text(WorldBank, 5, 80, "year")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(length(generated.names), 0L)
})

test_that('hiding the color legend works with a_scale_color(a_guide="none")',{
  viz$scatter <- viz$scatter+
    a_scale_color_discrete(a_guide="none")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, "population")
})

test_that('hiding the color legend works with a_guides(color="none")',{
  viz$scatter <- viz$scatter+
    a_guides(color="none")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, "population")
})

test_that('hiding all legends works with a_theme(legend.a_position="none")',{
  viz$scatter <- viz$scatter+
    a_theme(legend.a_position="none")
  info <- animint2dir(viz, open.browser=FALSE)
  generated.names <- names(info$plots$scatter$legend)
  expect_identical(generated.names, NULL)
})

error.types <-
  data.frame(x=1:3, status=c("correct", "false positive", "false negative"))

gg <- 
  a_plot(error.types)+
    a_geom_point(a_aes(x, x))+
    a_geom_tallrect(a_aes(xmin=x, xmax=x+0.5, fill=x),
                  color="black")

expected.legend.list <- 
  list(increasing=1:3,
       default=seq(3, 1, by=-0.5),
       decreasing=3:1)
    
test_that("renderer shows legend entries in correct order", {
  viz <-
    list(increasing=gg+
           a_scale_fill_continuous(breaks=1:3),
         decreasing=gg+
           a_scale_fill_continuous(breaks=3:1),
         default=gg)
  info <- animint2HTML(viz)
  ##sapply(info$plots, function(p)sapply(p$legend$x$entries, "[[", "a_label"))
  
  ## NOTE: it is important to test the renderer here (not the
  ## compiler) since maybe the order specified in the plot.json file
  ## is not the same as the order of appearance on the web page.

  ## The expected behavior is smaller numeric entries on the bottom of
  ## the legend by default, and if they are manually specified via
  ## breaks, we have:
  breaks <-
    c("top",
      "middle",
      "bottom")
  for(plot.name in names(expected.legend.list)){
    xpath <-
      sprintf('//td[@class="%s_legend"]//td[@class="legend_entry_a_label"]',
              plot.name)
    expected.entries <- expected.legend.list[[plot.name]]
    node.set <- getNodeSet(info$html, xpath)
    value.str <- sapply(node.set, xmlValue)
    value.num <- as.numeric(value.str)
    expect_equal(value.num, expected.entries)
  }
})
