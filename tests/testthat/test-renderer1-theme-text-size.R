acontext("legend and axis text size")

data(WorldBank, package = "animint2")
scatterFacet <- ggplot()+
    geom_point(aes(
    x=life.expectancy, y=fertility.rate, color=region),
    data=WorldBank)
viz <- list()
viz$scatterFacet <- scatterFacet
info <- animint2HTML(viz)

# Axes text size -------------
test_that("unspecified axis text size default to 11px", {
  style.value <-
    getStyleValue(info$html, '//g[@class="xaxis axis xaxis_1"]//g[@class="tick major"]//text', 
                  "font-size")
  expect_match(style.value, "11px")
})

# rel() only works for axis.text, axis.text.x, axis.text.y
test_that("specified axis text size with rel()", {
  s1 <- scatterFacet + theme(axis.text = element_text(size = rel(3)))
  viz1 <- list()
  viz1$one <- s1
  info1 <- animint2HTML(viz1)
  axis.x.style.value <-
    getStyleValue(info1$html, '//g[@class="xaxis axis xaxis_1"]//g[@class="tick major"]//text', 
                  "font-size")
  axis.y.style.value <-
    getStyleValue(info1$html, '//g[@class="yaxis axis yaxis_1"]//g[@class="tick major"]//text', 
                  "font-size")
  expect_match(axis.x.style.value, "33px")
  expect_match(axis.y.style.value, "33px")
})

test_that("if more than 1 element text size are defined, take the children node", {
  s2 <- scatterFacet + 
        theme(axis.text = element_text(size = 12),
              axis.text.x = element_text(size = 20))
  viz2 <- list()
  viz2$one <- s2
  info1 <- animint2HTML(viz2)
  axis.x.style.value <-
    getStyleValue(info1$html, '//g[@class="xaxis axis xaxis_1"]//g[@class="tick major"]//text', 
                  "font-size")
  axis.y.style.value <-
    getStyleValue(info1$html, '//g[@class="yaxis axis yaxis_1"]//g[@class="tick major"]//text', 
                  "font-size")
  expect_match(axis.x.style.value, "20px")
  expect_match(axis.y.style.value, "12px")
})

# legend text size -------------
test_that("unspecified legend title text size default to 16px", {
  style.value <-
    getStyleValue(info$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  expect_match(style.value, "16px")
})

test_that("unspecified legend label text size default to 16px", {
  style.value <-
    getStyleValue(info$html, '//table[@class="legend"]//tr[@id="plot_scatterFacet_region_variable_East_Asia_&_Pacific_(all_income_levels)"]//td[@class="legend_entry_label"]', 
                  "font-size")
  expect_match(style.value, "16px")
})

test_that("defined legend title text size", {
  l1 <- scatterFacet + 
        theme(legend.title = element_text(size=30))
  viz1 <- list()
  viz1$one <- l1
  info1 <- animint2HTML(viz1)
  style.value <-
    getStyleValue(info1$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  expect_match(style.value, "30px")
})

test_that("defined legend label text size", {
  l1 <- scatterFacet + 
        theme(legend.text = element_text(size=10))
  viz1 <- list()
  viz1$one <- l1
  info1 <- animint2HTML(viz1)
  style.value <-
    getStyleValue(info1$html, '//table[@class="legend"]//tr[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)"]//td[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)_label"]', 
                  "font-size")
  expect_match(style.value, "10px")
})