acontext("download status table")
viz <- animint(
  ggplot()+
    geom_point(aes(Sepal.Length, Sepal.Width), data=iris)
)
info <- animint2HTML(viz)
test_that("table has correct column headers", {
  table_headers <- getNodeSet(info$html, '//table[@id="download_status"]//th')
  header_text <- sapply(table_headers, xmlValue)
  expected <- c("geom", "files", "MB", "rows")
  expect_true(all(expected %in% header_text))
})
test_that("numeric columns are right-justified", {
  files_header <- getNodeSet(info$html, '//table[@id="download_status"]//th[@class="files"]')
  mb_header <- getNodeSet(info$html, '//table[@id="download_status"]//th[@class="MB"]')
  rows_header <- getNodeSet(info$html, '//table[@id="download_status"]//th[@class="rows"]')
  expect_equal(length(files_header), 1)
  expect_equal(length(mb_header), 1)
  expect_equal(length(rows_header), 1)
  expect_match(xmlGetAttr(files_header[[1]], "style"), "text-align.*right")
  expect_match(xmlGetAttr(mb_header[[1]], "style"), "text-align.*right")
  expect_match(xmlGetAttr(rows_header[[1]], "style"), "text-align.*right")
})
