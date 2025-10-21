acontext("NA separate lines")
library(data.table)

data(txhousing)

## ggplot2 draws separate lines when there are missing values.

## san.marcos <- subset(txhousing, city=="San Marcos")
## ggplot()+
##   geom_line(aes(x = date, y = median),
##             data=san.marcos)

viz <- animint(
  ggdata=ggplot(txhousing)+
    geom_path(aes(
      x = date, y = median, group = city),
      clickSelects="city",
      alpha = 0.6),
  selected=ggplot()+
    geom_path(aes(
      x = date, y = median, group = city),
      showSelected="city",
      chunk_vars="city",
      data=txhousing),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("no NA in tsv files", {
  geom1.tsv <- file.path("animint-htmltest", "geom1_path_ggdata_chunk1.tsv")
  geom1.data <- read.table(geom1.tsv, sep="\t", header=TRUE)
  expect_equal(sum(is.na(geom1.data)), 0)
})

test_that("no geom2 common chunk with group=city", {
  geom2.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk1.tsv")
  geom2.data <- read.table(geom2.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(geom2.data)), c("group","x","y"))
  common.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk_common.tsv")
  expect_false(file.exists(common.tsv))
})

test_that("three <path> rendered for highlighted San Marcos group=city", {
  xpath <- '//g[@class="geom1_path_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos group=city", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_path_selected"]//path')
  expect_equal(length(path.list), 3)
})

txdt <- data.table(txhousing)[, constant := "foo"]
viz <- animint(
  ggdata=ggplot(txdt)+
    geom_path(aes(
      x = date, y = median, group = city),
      clickSelects="city",
      alpha = 0.6),
  selected=ggplot()+
    geom_path(aes(
      x = date, y = median, group=1, color=constant),
      showSelected="city",
      chunk_vars="city",
      data=txdt),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("geom2 common chunk with group=1 and color common", {
  geom1.tsv <- file.path("animint-htmltest", "geom1_path_ggdata_chunk1.tsv")
  geom1.dt <- fread(geom1.tsv)
  expect_equal(sum(is.na(geom1.dt)), 0)
  plot.json <- file.path("animint-htmltest", "plot.json")
  json.list <- RJSONIO::fromJSON(plot.json)
  group_num <- json.list$geoms$geom2_path_selected$chunks[["San Marcos"]]
  geom.tsv <- sprintf("animint-htmltest/geom2_path_selected_chunk%d.tsv", group_num)
  geom.dt <- fread(geom.tsv)
  expect_identical(sort(names(geom.dt)), c("group", "na_group", "row_in_group", "y"))
  geom2.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk1.tsv")
  geom2.data <- read.table(geom2.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(geom2.data)), c("group", "y"))
  common.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk_common.tsv")
  common.data <- fread(common.tsv)
  expect_identical(sort(names(common.data)), c("colour", "group", "showSelected2", "x"))
})

test_that("three <path> rendered for highlighted San Marcos with group=1 and color common", {
  xpath <- '//g[@class="geom1_path_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos with group=1 and color common", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_path_selected"]//path')
  expect_equal(length(path.list), 3)
})

txdt <- data.table(txhousing)[, constant := "foo"]
viz <- animint(
  ggdata=ggplot(txdt)+
    geom_path(aes(
      x = date, y = median, group = city),
      clickSelects="city",
      alpha = 0.6),
  selected=ggplot()+
    geom_path(aes(
      x = date, y = median, color=constant),
      showSelected="city",
      chunk_vars="city",
      data=txdt),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("geom2 common chunk with no group and color common", {
  geom1.tsv <- file.path("animint-htmltest", "geom1_path_ggdata_chunk1.tsv")
  geom1.dt <- fread(geom1.tsv)
  expect_equal(sum(is.na(geom1.dt)), 0)
  plot.json <- file.path("animint-htmltest", "plot.json")
  json.list <- RJSONIO::fromJSON(plot.json)
  group_num <- json.list$geoms$geom2_path_selected$chunks[["San Marcos"]]
  geom.tsv <- sprintf("animint-htmltest/geom2_path_selected_chunk%d.tsv", group_num)
  geom.dt <- fread(geom.tsv)
  expect_identical(sort(names(geom.dt)), c("group", "na_group", "row_in_group", "y"))
  geom2.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk1.tsv")
  geom2.data <- read.table(geom2.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(geom2.data)), c("group", "y"))
  common.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk_common.tsv")
  common.data <- fread(common.tsv)
  expect_identical(sort(names(common.data)), c("colour", "group", "showSelected2", "x"))
})

test_that("three <path> rendered for highlighted San Marcos with no group and color common", {
  xpath <- '//g[@class="geom1_path_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos with no group and color common", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_path_selected"]//path')
  expect_equal(length(path.list), 3)
})

txdt <- data.table(txhousing)[
, median_NA := ifelse(anyNA(median), "some_missing", "none_missing")
, by=city][]
viz <- animint(
  ggdata=ggplot(txdt)+
    geom_path(aes(
      x = date, y = median, group = city),
      clickSelects="city",
      alpha = 0.6),
  selected=ggplot()+
    geom_path(aes(
      x = date, y = median, group=1, color=median_NA),
      showSelected="city",
      chunk_vars="city",
      data=txdt),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("geom2 common chunk ok with group=1 and only x common", {
  geom1.tsv <- file.path("animint-htmltest", "geom1_path_ggdata_chunk1.tsv")
  geom1.dt <- fread(geom1.tsv)
  expect_equal(sum(is.na(geom1.dt)), 0)
  plot.json <- file.path("animint-htmltest", "plot.json")
  json.list <- RJSONIO::fromJSON(plot.json)
  group_num <- json.list$geoms$geom2_path_selected$chunks[["San Marcos"]]
  geom.tsv <- sprintf("animint-htmltest/geom2_path_selected_chunk%d.tsv", group_num)
  geom.dt <- fread(geom.tsv)
  expect_identical(sort(names(geom.dt)), c("colour", "group", "na_group", "row_in_group", "showSelected2", "y"))
  geom2.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk1.tsv")
  geom2.data <- fread(geom2.tsv)
  expect_identical(sort(names(geom2.data)), c("colour", "group", "showSelected2", "y"))
  common.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk_common.tsv")
  common.data <- fread(common.tsv)
  common.expected <- txdt[city=="San Marcos", .(group=1, x=date)]
  expect_equal(common.data, common.expected)
})

test_that("three <path> rendered for highlighted San Marcos with group=1 and only x common", {
  xpath <- '//g[@class="geom1_path_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos with group=1 and only x common", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_path_selected"]//path')
  expect_equal(length(path.list), 3)
})
