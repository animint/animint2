acontext("legend and axis text size")

data(WorldBank, package = "animint2")
scatterFacet <- ggplot()+
    geom_point(aes(
    x=life.expectancy, y=fertility.rate, color=region),
    data=WorldBank)
viz <- list()
viz$scatterFacet <- scatterFacet
info <- animint2HTML(viz)

get_axes_style <- function(html){
  axis.style.list <- list()
  for(xy in c("x","y")){
    xpath <- sprintf(
      '//g[@class="%saxis axis %saxis_1"]//g[@class="tick major"]//text',
      xy, xy)
    axis.style.list[[xy]] <- getStyleValue(html, xpath, "font-size")
  }
  axis.style.list
}

# Axes text size -------------
test_that("unspecified axis text size default to 11pt", {
  axis.style.list <- get_axes_style(info$html)
  expect_match(axis.style.list$x, "11pt")
})

# rel() only works for axis.text, axis.text.x, axis.text.y
test_that("specified axis text size with rel()", {
  s <- scatterFacet + theme(axis.text = element_text(size = rel(3)))
  viz <- list()
  viz$one <- s
  info <- animint2HTML(viz)
  axis.style.list <- get_axes_style(info$html)
  # default size for 'text' is 11, so rel(3) * 11 = 33
  expect_match(axis.style.list$x, "33pt")
  expect_match(axis.style.list$y, "33pt")
})

test_that("if more than 1 element text size are defined, take the children node", {
  s <- scatterFacet + 
        theme(axis.text = element_text(size = 12),
              axis.text.x = element_text(size = 20))
  viz <- list()
  viz$one <- s
  info <- animint2HTML(viz)
  axis.style.list <- get_axes_style(info$html)
  expect_match(axis.style.list$x, "20pt")
  expect_match(axis.style.list$y, "12pt")
})

# legend text size -------------
test_that("unspecified legend title default to 11pt, and label text size 8.8pt", {
  title.size <-
    getStyleValue(info$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  label.size <-
    getStyleValue(info$html, '//table[@class="legend"]//tr[@id="plot_scatterFacet_region_variable_East_Asia_&_Pacific_(all_income_levels)"]//td[@class="legend_entry_label"]', 
                  "font-size")
  expect_match(title.size, "11pt")
  expect_match(label.size, "8.8pt")
})

test_that("defined legend title text size", {
  l <- scatterFacet + 
        theme(legend.title = element_text(size=30))
  viz <- list()
  viz$one <- l
  info <- animint2HTML(viz)
  style.value <-
    getStyleValue(info$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  expect_match(style.value, "30pt")
})

test_that("defined legend label text size", {
  l <- scatterFacet + 
        theme(legend.text = element_text(size=10))
  viz <- list()
  viz$one <- l
  info <- animint2HTML(viz)
  style.value <-
    getStyleValue(info$html, '//table[@class="legend"]//tr[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)"]//td[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)_label"]', 
                  "font-size")
  expect_match(style.value, "10pt")
})

test_that("specified legend title and label text size with rel()", {
  s <- scatterFacet + 
        theme(legend.text = element_text(size=rel(2)),
              legend.title = element_text(size=rel(2.5)))
  viz <- list()
  viz$one <- s
  info <- animint2HTML(viz)
  legend.text.size <-
    getStyleValue(info$html, '//table[@class="legend"]//tr[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)"]//td[@id="plot_one_region_variable_Europe_&_Central_Asia_(all_income_levels)_label"]', 
                  "font-size")
  legend.title.size <-
    getStyleValue(info$html, '//table[@class="legend"]//tr//th', 
                  "font-size")
  # parent text size for 'legend.text' is 11, so rel(2) * 11 = 22
  expect_match(legend.text.size, "22pt")
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
      data=df),
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

test_that("Warning for invalid character/string input ", {
  viz <- list(
    s=scatterFacet + theme(axis.text.x = element_text(size = "12p")))
  expect_warning(animint2HTML(viz), "axis.text.x is not numeric nor character ending with \'pt\' or \'px\', will be default 11pt")
})