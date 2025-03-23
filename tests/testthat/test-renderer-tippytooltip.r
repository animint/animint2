acontext("aes(tooltip)")

library(animint2)
data(WorldBank)

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
                      key=country),
                  clickSelects="country",
                  showSelected="year",
                  data=WorldBank)+
       geom_text(aes(life.expectancy, fertility.rate, label=country,
                     key=country),
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

# Test 1: Checking data-tippy-content for points (circles)
test_that("aes(tooltip) means show tooltip with data-tippy-content", {
  circle_xpath <- '//g[@class="geom1_point_scatter"]//circle[@data-tippy-content="India population 622232355"]'
  circle_node <- getNodeSet(info$html, circle_xpath)
  expect_equal(length(circle_node), 1, info = "Circle element for India should exist.")
  tooltip_text <- xmlGetAttr(circle_node[[1]], "data-tippy-content")
  expect_match(tooltip_text, "India population 622232355", info = "Tooltip text should match 'India population 622232355'.")
})

# Test 2: Checking aria-expanded for points (circles)
test_that("aria-expanded is initially false for points", {
  circle_xpath <- '//g[@class="geom1_point_scatter"]//circle[@data-tippy-content="India population 622232355"]'
  circle_node <- getNodeSet(info$html, circle_xpath)
  aria_expanded <- xmlGetAttr(circle_node[[1]], "aria-expanded")
  expect_equal(aria_expanded, "false", info = "aria-expanded should be 'false' initially.")
})

# Test 3: Checking data-tippy-content for bars (rects)
test_that("aes(tooltip) means show tooltip with data-tippy-content for bars", {
  rect_xpath <- '//g[@class="geom8_bar_bar"]//rect[@data-tippy-content="country Albania"]'
  rect_node <- getNodeSet(info$html, rect_xpath)
  expect_equal(length(rect_node), 1, info = "Rect element for Albania should exist.")
  tooltip_text <- xmlGetAttr(rect_node[[1]], "data-tippy-content")
  expect_match(tooltip_text, "country Albania", info = "Tooltip text should match 'country Albania'.")
})

# Test 4: Checking aria-expanded for bars (rects)
test_that("aria-expanded is initially false for bars", {
  rect_xpath <- '//g[@class="geom8_bar_bar"]//rect[@data-tippy-content="country Albania"]'
  rect_node <- getNodeSet(info$html, rect_xpath)
  aria_expanded <- xmlGetAttr(rect_node[[1]], "aria-expanded")
  expect_equal(aria_expanded, "false", info = "aria-expanded should be 'false' initially.")
})

# Test 5: Checking absence of tooltips for elements without aes(tooltip)
test_that("aes() means show no tooltip", {
  rect_xpath <- '//g[@class="geom4_rect_scatter"]//rect'
  rect_node <- getNodeSet(info$html, rect_xpath)
  expect_equal(length(rect_node), 1, info = "Rect element should exist.")
  tooltip_text <- xmlGetAttr(rect_node[[1]], "data-tippy-content", default = NA)
  expect_true(is.na(tooltip_text), info = "data-tippy-content should not be present.")
})
