acontext("NA separate lines")

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

viz <- animint(
  ggdata=ggplot(txhousing)+
    geom_path(aes(
      x = date, y = median, group = city),
      clickSelects="city",
      alpha = 0.6),
  selected=ggplot()+
    geom_path(aes(
      x = date, y = median, group=1),
      showSelected="city",
      data=txhousing),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

##animint2:::getCommonChunk(data.table(txhousing)[, .(x=date, y=median, showSelected=city, group=1)], "showSelected", aes.list=list(group=1))

test_that("geom2 common chunk with group=1", {
  ##lines_dt <- fread(cmd="wc -l animint-htmltest/*tsv|grep geom2", col.names=c("lines","file"))
  geom1.tsv <- file.path("animint-htmltest", "geom1_path_ggdata_chunk1.tsv")
  geom1.dt <- fread(geom1.tsv)
  expect_equal(sum(is.na(geom1.dt)), 0)
  group_num <- geom1.dt[clickSelects=="San Marcos"]$group[1]
  geom.tsv <- sprintf("animint-htmltest/geom2_path_selected_chunk%d.tsv", group_num)
  geom.dt <- fread(geom.tsv)
  expect_identical(sort(names(geom.dt)), c("group", "na_group", "row_in_group", "y"))
  geom2.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk1.tsv")
  geom2.data <- read.table(geom2.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(geom2.data)), c("group", "y"))
  common.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk_common.tsv")
  common.data <- read.table(common.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(common.data)), c("group","x"))
})

test_that("three <path> rendered for highlighted San Marcos with group=1", {
  xpath <- '//g[@class="geom1_path_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos with group=1", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_path_selected"]//path')
  expect_equal(length(path.list), 3)
})

viz <- animint(
  ggdata=ggplot(txhousing)+
    geom_path(aes(
      x = date, y = median, group = city),
      clickSelects="city",
      alpha = 0.6),
  selected=ggplot()+
    geom_path(aes(
      x = date, y = median),
      showSelected="city",
      data=txhousing),
  first=list(city="San Marcos")
)
info <- animint2HTML(viz)

test_that("geom2 common chunk without aes(group)", {
  ##lines_dt <- fread(cmd="wc -l animint-htmltest/*tsv|grep geom2", col.names=c("lines","file"))
  geom1.tsv <- file.path("animint-htmltest", "geom1_path_ggdata_chunk1.tsv")
  geom1.dt <- fread(geom1.tsv)
  expect_equal(sum(is.na(geom1.dt)), 0)
  group_num <- geom1.dt[clickSelects=="San Marcos"]$group[1]
  geom.tsv <- sprintf("animint-htmltest/geom2_path_selected_chunk%d.tsv", group_num)
  geom.dt <- fread(geom.tsv)
  geom2.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk1.tsv")
  geom2.data <- read.table(geom2.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(geom2.data)), c("group", "na_group", "row_in_group", "y"))
  common.tsv <- file.path("animint-htmltest", "geom2_path_selected_chunk_common.tsv")
  common.data <- read.table(common.tsv, sep="\t", header=TRUE)
  expect_identical(sort(names(common.data)), c("group","x"))
})

test_that("three <path> rendered for highlighted San Marcos without aes(group)", {
  xpath <- '//g[@class="geom1_path_ggdata"]//path'
  path.list <- getNodeSet(info$html, xpath)
  opacity.str <- getStyleValue(info$html, xpath, "opacity")
  opacity.num <- as.numeric(opacity.str)
  hilite.list <- path.list[opacity.num == 0.6]
  expect_equal(length(hilite.list), 3)
})

test_that("three <path> rendered for selected San Marcos without aes(group)", {
  path.list <- getNodeSet(info$html, '//g[@class="geom2_path_selected"]//path')
  expect_equal(length(path.list), 3)
})



