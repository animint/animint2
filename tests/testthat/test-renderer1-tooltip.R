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
                      tooltip=paste(country, "population", population), id = country,
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country, tooltip = country, id=paste0("text_",country),
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
tooltip.xpath <- '//div[@class="animint-tooltip"]'
test_that("animint-tooltip div exists with correct initial state", {
  tooltip_div <- getNodeSet(info$html, tooltip.xpath)
  expect_equal(length(tooltip_div), 1)
  # Check initial opacity is 0
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
})

test_that("tooltip shows correct content for rect", {
  # Get rect position on viewport
  rect_position <- get_element_bbox('rect.geom')
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = rect_position$left, y = rect_position$top)
  Sys.sleep(0.3)
  tooltip_div <- getNodeSet(getHTML(), tooltip.xpath)[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  # Verify tooltip contains expected content
  expect_match(tooltip_text, "187 not NA in 1975")
  # Verify that tooltip opacity is now 1 (tooltip shown)
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "1")
  # Move mouse away to clean up
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  Sys.sleep(0.2)
  # Verify that tooltip hides correctly after mouseout
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
})

test_that("tooltip shows correct content for point", {
  # Get circle position on viewport
  circle_position <- get_element_bbox('circle#China')
  # Hover over center of the circle
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = circle_position$center_x,
    y = circle_position$center_y
  )
  Sys.sleep(0.3)
  tooltip_div <- getNodeSet(getHTML(), tooltip.xpath)[[1]]
  tooltip_text <- xmlValue(tooltip_div)
  # Verify tooltip contains expected content
  expect_match(tooltip_text, "China population 916395000")
  # Verify that tooltip opacity is now 1 (tooltip shown)
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "1")
  # Move mouse away to clean up
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  Sys.sleep(0.2)
  # Verify that tooltip hides correctly after mouseout
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
})

test_that("tooltip shows correct content for geom_text", {
  clickID('China') # Select the circle corresponding to China for highlighting text
  Sys.sleep(0.2)
  # Get text position on viewport
  text_position <- get_element_bbox('text#text_China')
  remDr$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = text_position$center_x,
    y = text_position$center_y
  )
  Sys.sleep(0.2)
  tooltip_div <- getNodeSet(getHTML(), tooltip.xpath)[[1]]
  tooltip_text <- xmlValue(tooltip_div) 
  # Verify tooltip contains expected content
  expect_match(tooltip_text, "China")
  # Verify that tooltip opacity is now 1 (tooltip shown)
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "1")
  # Move mouse away to clean up
  remDr$Input$dispatchMouseEvent(type = "mouseMoved", x = 0, y = 0)
  # Verify that tooltip hides correctly after mouseout
  opacity <- getStyleValue(getHTML(), tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
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
  tooltip_div <- getNodeSet(info$html, tooltip.xpath)
  expect_equal(length(tooltip_div), 1)
  # Opacity is initially 0 when tooltip is hidden
  opacity <- getStyleValue(info$html, tooltip.xpath, "opacity")
  expect_identical(opacity, "0")
})
