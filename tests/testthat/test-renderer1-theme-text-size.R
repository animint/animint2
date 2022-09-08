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
  expect_match(style.value, "11pt")
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
  # default size for 'text' is 11, so rel(3) * 11 = 33
  expect_match(axis.x.style.value, "33pt")
  expect_match(axis.y.style.value, "33pt")
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
  expect_match(axis.x.style.value, "20pt")
  expect_match(axis.y.style.value, "12pt")
})

# legend text size -------------
test_that("unspecified legend title and label text size default to 16px", {
  title.size <-
    getStyleValue(info$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  label.size <-
    getStyleValue(info$html, '//table[@class="legend"]//tr[@id="plot_scatterFacet_region_variable_East_Asia_&_Pacific_(all_income_levels)"]//td[@class="legend_entry_label"]', 
                  "font-size")
  expect_match(title.size, "16pt")
  expect_match(label.size, "16pt")
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
  expect_match(style.value, "30pt")
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
  expect_match(style.value, "10pt")
})

test_that("specified legend title and label text size with rel()", {
  s1 <- scatterFacet + 
        theme(legend.text = element_text(size=rel(2)),
              legend.title = element_text(size=rel(2.5)))
  viz1 <- list()
  viz1$one <- s1
  info1 <- animint2HTML(viz1)
  legend.text.size <-
    getStyleValue(info1$html, '//table[@class="legend"]//tr[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)"]//td[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)_label"]', 
                  "font-size")
  legend.title.size <-
    getStyleValue(info1$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  # parent text size for 'legend.text' is 11, so rel(2) * 11 = 22
  expect_match(legend.text.size, "22pt")
  # rel(2.5) * 16 = 24
  expect_match(legend.title.size, "27.5pt")
})

## TDH default theme test, 1 Sep 2022.
y <- 1:2
df <- data.frame(y, text=paste("category", y))
viz <- animint(
  default=ggplot()+
    ggtitle("No theme specified")+
    geom_text(aes(
      0,y,label=text,color=text),
      data=df),
  theme=ggplot()+
    ggtitle("theme_grey()")+
    theme_grey()+
    geom_text(aes(
      0,y,label=text,color=text),
      data=df),
  sizeNum=ggplot()+
    ggtitle("theme_grey()+theme(legend.text)")+
    theme_grey()+
    theme(legend.text=element_text(size=16))+
    geom_text(aes(
      0,y,label=text,color=text),
      data=df)+
  sizePx=ggplot()+
    ggtitle("theme_grey()+theme(legend.text)")+
    theme_grey()+
    theme(legend.text=element_text(size="16px"))+
    geom_text(aes(
      0,y,label=text,color=text),
      data=df))
info <- animint2HTML(viz)
test_that("theme_grey legend entry text size is 16px", {
  size.list <- list()
  for(plot.name in names(viz)){
    selector <- sprintf(
      '//td[@id="plot_%s_text_variable_category_1_label"]', 
      plot.name)
    size.list[[plot.name]] <- getStyleValue(
      info$html, selector, "font-size")
  }
  expect_match(size.list$default, "8.8pt")
  expect_match(size.list$theme, "8.8pt")
  expect_match(size.list$sizeNum, "16pt")
  expect_match(size.list$sizePx, "16px")
})
