acontext("download status table")
viz <- animint(
  ggplot()+
    geom_point(aes(Sepal.Length, Sepal.Width), data=iris)
)
info <- animint2HTML(viz)
test_that("table has correct column headers", {
  table_headers <- getNodeSet(info$html, '//table[contains(@id,"_download_status")]//th')
  header_text <- sapply(table_headers, xmlValue)
  expected <- c("geom", "files", "disk", "rows")
  expect_true(all(expected %in% header_text))
})
test_that("numeric columns are right-justified", {
  files_header <- getNodeSet(info$html, '//table[contains(@id,"_download_status")]//th[@class="files"]')
  disk_header <- getNodeSet(info$html, '//table[contains(@id,"_download_status")]//th[@class="disk"]')
  rows_header <- getNodeSet(info$html, '//table[contains(@id,"_download_status")]//th[@class="rows"]')
  expect_equal(length(files_header), 1)
  expect_equal(length(disk_header), 1)
  expect_equal(length(rows_header), 1)
  expect_match(xmlGetAttr(files_header[[1]], "style"), "text-align.*right")
  expect_match(xmlGetAttr(disk_header[[1]], "style"), "text-align.*right")
  expect_match(xmlGetAttr(rows_header[[1]], "style"), "text-align.*right")
})
test_that("download status table displays correct content format", {
  # Get all table cells from the download status table
  table_cells <- getNodeSet(info$html, '//table[contains(@id,"_download_status")]//td')
  cell_text <- sapply(table_cells, xmlValue)
  
  # Should have cells for: geom name, files (1 / 1), disk (KiB/MiB), rows (with commas)
  # Files column should show "downloaded / total" format
  files_pattern <- "^[0-9]+ / [0-9]+$"
  files_cells <- grep(files_pattern, cell_text, value = TRUE)
  expect_true(length(files_cells) > 0, "Should have files column with 'downloaded / total' format")
  
  # Disk column should show bytes with KiB or MiB units
  disk_pattern <- "[0-9]+(\\.[0-9]+)? (KiB|MiB) / [0-9]+(\\.[0-9]+)? (KiB|MiB)"
  disk_cells <- grep(disk_pattern, cell_text, value = TRUE)
  expect_true(length(disk_cells) > 0, "Should have disk column with KiB/MiB units")
  
  # Rows column should show numbers (may have commas for large numbers)
  # Pattern allows digits with optional commas: 150 or 10,066
  rows_pattern <- "^[0-9,]+ / [0-9,]+$"
  rows_cells <- grep(rows_pattern, cell_text, value = TRUE)
  expect_true(length(rows_cells) > 0, "Should have rows column with numeric format")
})
