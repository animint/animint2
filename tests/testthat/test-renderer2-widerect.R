acontext("a_geom_widerect")

recommendation <- data.frame(
  min.C=21,
  max.C=23)
set.seed(1)
temp.time <- data.frame(
  time=strptime(paste0("2015-10-", 1:31), "%Y-%m-%d"),
  temp.C=rnorm(31))

viz <- list(
  gg=a_plot()+
    a_theme_bw()+
    a_theme_animint(height=200, width=2000)+
    a_geom_widerect(a_aes(ymin=min.C, ymax=max.C),
                  color=NA,
                  fill="grey",
                  data=recommendation)+
    a_geom_line(a_aes(time, temp.C),
              data=temp.time)
  )

info <- animint2HTML(viz)

getBounds <- function(a_geom.class){
  script.txt <- sprintf('return document.getElementsByClassName("%s")[0].getBoundingClientRect()', a_geom.class)
  remDr$executeScript(script.txt)
}

test_that("bottom of widerect is above line", {
  rect.bounds <- getBounds("a_geom1_widerect_gg")
  line.bounds <- getBounds("a_geom2_line_gg")
  expect_less_than(rect.bounds$bottom, line.bounds$top)
})

data(WorldBank, package = "animint2")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
BOTH <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}
TS <- function(df)BOTH(df, "Years", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS2 <- function(df)BOTH(df, "Fertility rate", "Years")
years <- unique(not.na[, "year", drop=FALSE])
years$status <- ifelse(years$year %% 2, "odd", "even")
wb.facets <-
  list(ts=a_plot()+
       xlab("")+
       a_geom_tallrect(a_aes(xmin=year-1/2, xmax=year+1/2,
                         linetype=status),
                     clickSelects="year",
                     data=TS(years), alpha=1/2)+
       a_theme_bw()+
       a_theme_animint(width=1000, height=800)+
       a_theme(panel.margin=grid::unit(0, "lines"))+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region,
                     id = country),
                 clickSelects="country",
                 data=TS(not.na), size=4, alpha=3/5)+
       a_geom_point(a_aes(year, life.expectancy, color=region, size=population),
                  clickSelects="country",
                  showSelected="country",
                  data=TS(not.na))+

       a_geom_path(a_aes(fertility.rate, year, group=country, colour=region),
                 clickSelects="country",
                 data=TS2(not.na), size=4, alpha=3/5)+
       a_geom_point(a_aes(fertility.rate, year, color=region, size=population),
                  showSelected="country", clickSelects="country",
                  data=TS2(not.na))+
       a_geom_widerect(a_aes(ymin=year-1/2, ymax=year+1/2,
                         linetype=status,
                         id=paste0("year", year)),
                     clickSelects="year",
                     data=TS2(years), alpha=1/2)+

       a_geom_point(a_aes(fertility.rate, life.expectancy,
                      colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=SCATTER(not.na))+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year"),
                 clickSelects="country",
                 data=SCATTER(not.na))+
       a_scale_size_animint(breaks=10^(5:9))+
       a_facet_grid(side ~ top, scales="free")+
       a_geom_text(a_aes(5, 85, a_label=paste0("year = ", year)),
                 showSelected="year",
                 data=SCATTER(years)),

       bar=a_plot()+
       a_theme_animint(height=2400)+
       a_geom_bar(a_aes(country, life.expectancy, fill=region,
                    key=country, id=gsub(" ", "_", country)),
                showSelected="year", clickSelects="country",
                data=not.na, a_stat="identity", a_position="identity")+
       a_coord_flip(),

       time=list(variable="year", ms=2000),
       duration=list(year=2000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection, facets)")

info <- animint2HTML(wb.facets)

rect.list <-
  getNodeSet(info$html, '//svg[@id="plot_ts"]//rect[@class="border_rect"]')
expect_equal(length(rect.list), 4)
at.mat <- sapply(rect.list, xmlAttrs)

test_that("three unique border_rect x values (no horiz space)", {
  left.vec <- as.numeric(at.mat["x", ])
  width.vec <- as.numeric(at.mat["width", ])
  right.vec <- left.vec + width.vec
  x.values <- unique(c(left.vec, right.vec))
  expect_equal(length(x.values), 3)
})

test_that("three unique border_rect y values (no vert space)", {
  top.vec <- as.numeric(at.mat["y", ])
  height.vec <- as.numeric(at.mat["height", ])
  bottom.vec <- top.vec + height.vec
  y.values <- unique(c(top.vec, bottom.vec))
  expect_equal(length(y.values), 3)
})

line.xpath <- '//g[@class="a_geom2_line_ts"]//g[@class="PANEL4"]//path'
opacityPattern <-
  paste0("opacity:",
         "(?<value>.*?)",
         ";")

test_that("line opacity initially 0.1 or 0.6", {

  node.set <- getNodeSet(info$html, line.xpath)
  opacity.list <- list()
  for(node.i in seq_along(node.set)){
    node <- node.set[[node.i]]
    a.vec <- xmlAttrs(node)
    style.str <- a.vec[["style"]]
    opacity.mat <- str_match_perl(style.str, opacityPattern)
    node.id <- a.vec[["id"]]
    opacity.list[[node.id]] <- as.numeric(opacity.mat[, "value"])
  }
  opacity.vec <- do.call(c, opacity.list)

  selected.computed <- as.numeric(opacity.vec[wb.facets$first$country])
  selected.expected <- rep(0.6, length(selected.computed))
  expect_equal(selected.computed, selected.expected)

  unselected.computed <-
    as.numeric(opacity.vec[!names(opacity.vec) %in% wb.facets$first$country])
  unselected.expected <- rep(0.1, length(unselected.computed))
  expect_equal(unselected.computed, unselected.expected)

})

dasharrayPattern <-
  paste0("stroke-dasharray:",
         "(?<value>.*?)",
         ";")

rect.xpaths <-
  c('//g[@class="a_geom6_widerect_ts"]//g[@class="PANEL1"]//rect',
    '//g[@class="a_geom1_tallrect_ts"]//g[@class="PANEL4"]//rect')

test_that("wide/tallrect renders a <rect> for every year", {
  for(rect.xpath in rect.xpaths){
    node.set <- getNodeSet(info$html, rect.xpath)
    expect_equal(length(node.set), nrow(years))
    style.list <- list()
    for(node.i in seq_along(node.set)){
      node <- node.set[[node.i]]
      a.vec <- xmlAttrs(node)
      style.list[[node.i]] <- a.vec[["style"]]
      sizes <- as.numeric(a.vec[c("height", "width")])
      expect_true(all(sizes > 0))
    }
    style.vec <- do.call(c, style.list)
    dash.mat <- str_match_perl(style.vec, dasharrayPattern)
    ## Use paste() to treat NA as a value instead of ignoring it.
    dash.table <- table(paste(dash.mat[, "value"]))
    ## There should be 2 unique values of stoke-dasharray.
    expect_equal(length(dash.table), 2)
  }
})

getYear <- function(){
  node.set <- getNodeSet(getHTML(), '//g[@class="a_geom9_text_ts"]//text')
  expect_equal(length(node.set), 1)
  value <- xmlValue(node.set[[1]])
  sub("year = ", "", value)
}

test_that("animation updates", {
  old.year <- getYear()
  Sys.sleep(5) #wait for two animation frames.
  new.year <- getYear()
  expect_true(old.year != new.year)
})

clickID("plot_show_hide_animation_controls")

test_that("pause stops animation", {
  clickID("play_pause")
  old.year <- getYear()
  Sys.sleep(3)
  new.year <- getYear()
  expect_true(old.year == new.year)
})

test_that("play restarts animation", {
  old.year <- getYear()
  clickID("play_pause")
  Sys.sleep(5)
  new.year <- getYear()
  expect_true(old.year != new.year)
})

test_that("pause stops animation (second time)", {
  clickID("play_pause")
  old.year <- getYear()
  Sys.sleep(3)
  new.year <- getYear()
  expect_true(old.year == new.year)
})

clickID("plot_ts_status_variable_even")
clickID("plot_ts_status_variable_odd")
html.no.rects <- getHTML()

test_that("clicking status legend hides tallrects", {
  for(rect.xpath in rect.xpaths){
    node.set <- getNodeSet(html.no.rects, rect.xpath)
    expect_equal(length(node.set), 0)
  }
})

test_that("clicking status legend does not hide text", {
  node.set <-
    getNodeSet(html.no.rects,
               '//g[@class="a_geom9_text_ts"]//text[@class="a_geom"]')
  expect_equal(length(node.set), 1)
})

clickID("plot_ts_status_variable_even")
clickID("plot_ts_status_variable_odd")
html.with.rects <- getHTML()

test_that("clicking status legend brings back tallrects", {
  for(rect.xpath in rect.xpaths){
    node.set <- getNodeSet(html.with.rects, rect.xpath)
    expect_equal(length(node.set), nrow(years))
    style.list <- list()
    for(node.i in seq_along(node.set)){
      node <- node.set[[node.i]]
      a.vec <- xmlAttrs(node)
      style.list[[node.i]] <- a.vec[["style"]]
      sizes <- as.numeric(a.vec[c("height", "width")])
      expect_true(all(sizes > 0))
    }
    style.vec <- do.call(c, style.list)
    dash.mat <- str_match_perl(style.vec, dasharrayPattern)
    ## Use paste() to treat NA as a value instead of ignoring it.
    dash.table <- table(paste(dash.mat[, "value"]))
    ## There should be 2 unique values of stoke-dasharray.
    expect_equal(length(dash.table), 2)
  }
})

test_that("play restarts animation (second time)", {
  old.year <- getYear()
  clickID("play_pause")
  Sys.sleep(5)
  new.year <- getYear()
  expect_true(old.year != new.year)
})

legend.td.xpath <-
  '//tr[@class="region_variable"]//td[@class="legend_entry_a_label"]'
rects_and_legends <- function(){
  html <- getHTML()
  list(rects=getNodeSet(html, '//rect[@id="United_States"]'),
       legends=getStyleValue(html, legend.td.xpath, "opacity"))
}
test_that("clicking legend removes/adds countries", {
  before <- rects_and_legends()
  expect_equal(length(before$rects), 1)
  expect_equal(sum(before$legends=="1"), 14)
  expect_equal(sum(before$legends=="0.5"), 0)
  clickID("plot_ts_region_variable_North_America")
  Sys.sleep(1)
  oneclick <- rects_and_legends()
  expect_equal(length(oneclick$rects), 0)
  expect_equal(sum(oneclick$legends=="1"), 12)
  expect_equal(sum(oneclick$legends=="0.5"), 2)
  clickID("plot_ts_region_variable_North_America")
  Sys.sleep(1)
  twoclicks <- rects_and_legends()
  expect_equal(length(twoclicks$rects), 1)
  expect_equal(sum(twoclicks$legends=="1"), 14)
  expect_equal(sum(twoclicks$legends=="0.5"), 0)
})

e <- remDr$findElement("id", "updates_ms")
e$clickElement()
e$clearElement()
e$sendKeysToElement(list("3000", key="enter"))

test_that("pause stops animation (third time)", {
  clickID("play_pause")
  old.year <- getYear()
  Sys.sleep(4)
  new.year <- getYear()
  expect_true(old.year == new.year)
})

e <- remDr$findElement("class name", "show_hide_selector_widgets")
e$clickElement()
s.tr <- remDr$findElement("class name", "year_variable_selector_widget")
s.div <- s.tr$findChildElement("class name", "selectize-input")
s.div$clickElement()
remDr$sendKeysToActiveElement(list(key="backspace"))
remDr$sendKeysToActiveElement(list("1962"))
remDr$sendKeysToActiveElement(list(key="enter"))
Sys.sleep(1)

test_that("typing into selectize widget changes year to 1962", {
  current.year <- getYear()
  expect_identical(current.year, "1962")
})

s.div$clickElement()
remDr$sendKeysToActiveElement(list(key="down_arrow"))
remDr$sendKeysToActiveElement(list(key="enter"))
Sys.sleep(1)

test_that("down arrow key changes year to 1963", {
  current.year <- getYear()
  expect_identical(current.year, "1963")
})

getCountries <- function(){
  country.labels <- getNodeSet(getHTML(), '//g[@class="a_geom8_text_ts"]//text')
  sapply(country.labels, xmlValue)
}

test_that("initial countries same as first", {
  country.vec <- getCountries()
  expect_identical(sort(country.vec), sort(wb.facets$first$country))
})

s.tr <- remDr$findElement("class name", "country_variable_selector_widget")
s.div <- s.tr$findChildElement("class name", "selectize-input")
s.div$clickElement()
remDr$sendKeysToActiveElement(list("Afg"))
remDr$sendKeysToActiveElement(list(key="enter"))
Sys.sleep(1)

test_that("Afg autocompletes to Afghanistan", {
  country.vec <- getCountries()
  expected.countries <- c("United States", "Vietnam", "Afghanistan")
  expect_identical(sort(country.vec), sort(expected.countries))
})

div.list <- s.tr$findChildElements("class name", "item")
names(div.list) <- sapply(div.list, function(e)e$getElementText()[[1]])
us.div <- div.list[["United States"]]
us.div$clickElement()
remDr$sendKeysToActiveElement(list(key="backspace"))
Sys.sleep(1)

test_that("backspace removes US from selected countries", {
  country.vec <- getCountries()
  expected.countries <- c("Vietnam", "Afghanistan")
  expect_identical(sort(country.vec), sort(expected.countries))
})

getWidth <- function(){
  node.set <-
    getNodeSet(getHTML(), '//g[@class="a_geom10_bar_bar"]//rect[@id="Vietnam"]')
  expect_equal(length(node.set), 1)
  alist <- xmlAttrs(node.set[[1]])
  alist[["width"]]
}

test_that("middle of transition != after when duration=2000", {
  clickID("year1960")
  Sys.sleep(1)
  before.width <- getWidth()
  clickID("year2010")
  during.width <- getWidth()
  Sys.sleep(0.1)
  after.width <- getWidth()
  rbind(before=before.width,
        during=during.width,
        after=after.width)
  expect_true(during.width != after.width)
})

e <- remDr$findElement("id", "plot_duration_ms_year")
e$clickElement()
e$clearElement()
e$sendKeysToElement(list("0", key="enter"))
Sys.sleep(1)

test_that("middle of transition == after when duration=0", {
  clickID("year1960")
  Sys.sleep(1)
  before.width <- getWidth()
  clickID("year2010")
  during.width <- getWidth()
  Sys.sleep(0.1)
  after.width <- getWidth()
  rbind(before=before.width,
        during=during.width,
        after=after.width)
  expect_true(before.width != after.width)
  expect_true(during.width == after.width)
})
