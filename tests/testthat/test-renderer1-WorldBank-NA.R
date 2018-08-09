acontext("WorldBank-NA")

data(WorldBank, package = "animint2")

## This example is good because it uses constancy
## http://bost.ocks.org/mike/constancy/
no.time <-
  list(scatter=a_plot()+
       a_geom_point(a_aes(life.expectancy, fertility.rate,
                      colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  showSelected="year",
                  clickSelects="country",
                  data=WorldBank)+
       a_geom_text(a_aes(life.expectancy, fertility.rate, a_label=country,
                     key=country), # also use key here!
                 showSelected=c("country", "year"),
                 data=WorldBank)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       
       ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),

       bar=a_plot()+
       a_theme_animint(height=2400)+
       a_geom_bar(a_aes(country, life.expectancy, fill=region,
                    key=country),
                showSelected="year", clickSelects="country",
                data=WorldBank, a_stat="identity", a_position="identity")+
       a_coord_flip(),
       
       duration=list(year=1000),
       
       first=list(year=1975, country="United States"),
       
       title="World Bank data (single selection)")

bar.attributes <- function(html){
  node.set <-
    getNodeSet(info$html, '//g[@class="a_geom6_bar_bar"]//rect')
  sapply(node.set, xmlAttrs)
}

info <- animint2HTML(no.time)

chunk1.tsv <- file.path("animint-htmltest", "a_geom6_bar_bar_chunk1.tsv")
chunk1 <- read.table(chunk1.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")

test_that("chunk1 contains expected columns", {
  expect_identical(sort(names(chunk1)), sort(c("xmax", "group")))
})

test_that("chunk1 does not contain NA", {
  not.missing <- !is.na(chunk1)
  expect_true(all(not.missing))
})

common.tsv <- file.path("animint-htmltest", "a_geom6_bar_bar_chunk_common.tsv")
common <- read.table(common.tsv, sep="\t", header=TRUE,
                     comment.char="", quote="")

test_that("common chunk contains expected columns", {
  expected.cols <-
    c("ymin", "ymax", "xmin", "fill", "key",
      "clickSelects", "showSelectedlegendfill",
      "group")
  expect_identical(sort(names(common)), sort(expected.cols))
})

test_that("common chunk does not contain NA", {
  not.missing <- !is.na(common)
  expect_true(all(not.missing))
})

test_that("bars render without time", {
  at.mat <- bar.attributes(info$html)
  num.vec <- as.numeric(at.mat[c("x", "width", "y", "height"), ])
  expect_true(0 < ncol(at.mat))
  expect_true(all(is.finite(num.vec)))
})

with.time <- no.time
with.time$time <- list(variable="year", ms=3000)
info <- animint2HTML(with.time)

test_that("bars render with time", {
  at.mat <- bar.attributes(info$html)
  num.vec <- as.numeric(at.mat[c("x", "width", "y", "height"), ])
  expect_true(0 < ncol(at.mat))
  expect_true(all(is.finite(num.vec)))
})
