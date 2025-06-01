acontext("aes(tooltip)")

data(WorldBank, package = "animint2")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
country.counts <- table(not.na$year)
years <- data.frame(year=as.numeric(names(country.counts)),
                    countries=as.numeric(country.counts))
viz <-
  list(scatter=ggplot()+
       geom_point(aes(life.expectancy, fertility.rate,
                      colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year"),
                 data=WorldBank)+
       scale_size_animint(breaks=10^(5:9))+
       geom_rect(aes(xmin=45, xmax=70,
                     ymin=8, ymax=10,
                     tooltip=paste(countries, "not NA in", year),
                     key=year),
                 showSelected="year",
                 data=years, color="yellow")+
       geom_rect(aes(xmin=35, xmax=40,
                     ymin=2, ymax=2.5,
                     key=year),
                 showSelected="year",
                 data=years, color="orange")+
       geom_text(aes(55, 9, label=paste("year =", year),
                     key=year),
                 showSelected="year",
                 data=years),

       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),

       bar=ggplot()+
       theme_animint(height=2400)+
       geom_bar(aes(country, life.expectancy, fill=region, key=year),
                showSelected="year", clickSelects="country",
                data=WorldBank, stat="identity", position="identity")+
       coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country="United States"),
       title="World Bank data (single selection)")

subset(WorldBank, country=="United States" & year == 1975)$population
subset(years, year==1975)

info <- animint2HTML(viz)

test_that("animint-tooltip div exists with correct initial state", {
  tooltip_div <- getNodeSet(info$html, '//div[@class="animint-tooltip"]')
  expect_equal(length(tooltip_div), 1)
  # Check initial opacity is 0
  style <- xmlGetAttr(tooltip_div[[1]], "style")
  expect_match(style, "opacity: 0;")
})

test_that("tooltip shows correct content for rect", {
  # Find the rectangle element
  rect_node <- getNodeSet(
    info$html, 
    '//g[@class="geom3_rect_scatter"]//rect'
  )[[1]]
  
  # Get coordinates for hover simulation
  rect_x <- as.numeric(xmlGetAttr(rect_node, "x"))
  rect_y <- as.numeric(xmlGetAttr(rect_node, "y"))
  rect_width <- as.numeric(xmlGetAttr(rect_node, "width"))
  rect_height <- as.numeric(xmlGetAttr(rect_node, "height"))
  
  # Calculate center point
  center_x <- rect_x + (rect_width / 2)
  center_y <- rect_y + (rect_height / 2)
  
  # Simulate hover over rectangle center
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = center_x, y = center_y)
  Sys.sleep(0.5) # Wait for tooltip
  
  # Check tooltip content matches expected
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  expect_match(tooltip_text, "187 not NA in 1975")
  
  # Clean up - move mouse away
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  Sys.sleep(0.5)
})

test_that("tooltip shows correct content for point", {
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved", 
    x = 230.01787959856264, #coordinates for the circle corresponding to Country Myanmar
    y = 177.9050016888368
  )
  Sys.sleep(0.5)
  
  # Check tooltip contains "year"
  tooltip_div <- getNodeSet(getHTML(), '//div[@class="animint-tooltip"]')[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  expect_match(tooltip_text, "Myanmar population 30640635")
  
  # Clean up - move mouse away
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
})

# Test with href
WorldBank1975 <- WorldBank[WorldBank$year == 1975, ]
NotNA1975 <- subset(not.na, year==1975)
ex_plot <- ggplot() +
  geom_point(aes(fertility.rate, life.expectancy, color = region,
                 tooltip = country, href = "https://github.com"),
             data = WorldBank1975)

viz <- list(ex = ex_plot)
info <- animint2HTML(viz)

test_that("tooltip div exists with href elements", {
  tooltip_div <- getNodeSet(info$html, '//div[@class="animint-tooltip"]')
  expect_equal(length(tooltip_div), 1)
  expect_match(xmlGetAttr(tooltip_div[[1]], "style"), "opacity: 0;")
})
