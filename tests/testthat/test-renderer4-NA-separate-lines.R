acontext("NA separate lines")

data(txhousing)

## ggplot2 draws separate lines when there are missing values.

## san.marcos <- subset(txhousing, city=="San Marcos")
## a_plot()+
##   a_geom_line(a_aes(x = date, y = median),
##             data=san.marcos)

viz <- list(
  ggdata=a_plot(txhousing)+
    a_geom_line(a_aes(x = date, y = median, group = city), 
              clickSelects="city",
              alpha = 0.6),
  selected=a_plot()+
    a_geom_line(a_aes(x = date, y = median, group = city),
              showSelected="city",
              data=txhousing),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("no NA in tsv files", {
  a_geom1.tsv <- file.path("animint-htmltest", "a_geom1_line_ggdata_chunk1.tsv")
  a_geom1.data <- read.table(a_geom1.tsv, sep="\t", header=TRUE)
  expect_equal(sum(is.na(a_geom1.data)), 0)
})

test_that("three <path> rendered for highlighted San Marcos", {
  xpath <- '//g[@class="a_geom1_line_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos", {
  path.list <- getNodeSet(info$html, '//g[@class="a_geom2_line_selected"]//path')
  expect_equal(length(path.list), 3)
})
