library(testthat)
library(animint2)
library(data.table)

context("Rosling Bubbles Hard Test")

#setup dummy data for testing transitions
set.seed(42)
n_individuals <- 10
n_years <- 5

dt_list <- lapply(1:n_years, function(k) {
  data.table(
    id = factor(paste("Subject", 1:n_individuals)),
    year = k,
    x = rnorm(n_individuals, mean = k/5, sd = 2), 
    y = rnorm(n_individuals, mean = k/10, sd = 2),
    size1 = abs(rnorm(n_individuals, 2, 0.5)),
    size2 = abs(rnorm(n_individuals, 2, 0.5)),
    label = paste("Subj:", 1:n_individuals)
  )
})
rosling_data <- rbindlist(dt_list)

#data for the year label background
year_labels <- unique(rosling_data[, .(year)])
year_labels[, year_text := as.character(year)]
year_labels[, key_id := 1] 

#viz
viz <- list(
  bubbles = ggplot() +
    geom_text(data = year_labels,
              aes(x = 5, y = 2.5, label = year_text, key = key_id),
              showSelected = "year",
              size = 40, color = "grey92") +
    geom_point(data = rosling_data,
               aes(x = x, y = y, size = size1, fill = id, key = id),
               showSelected = "year",
               shape = 21, color = "black", alpha = 0.7) +
    scale_size_continuous(range = c(5, 20)) +
    theme_bw() +
    ggtitle("Bubbles: Rosling Test"),
  
  rects = ggplot() +
    geom_rect(data = rosling_data,
              aes(xmin = x - size1, xmax = x + size1,
                  ymin = y - size2, ymax = y + size2,
                  fill = id, key = id),
              showSelected = "year",
              color = "black", alpha = 0.7) +
    theme_bw() + 
    ggtitle("Rects: Dimension Test"),
  
  time = list(variable = "year", ms = 1000),
  duration = list(year = 1000)
)

#test-case
test_that("compiler correctly generates tsv chunks for point and rect layers", {
  
  test_path <- file.path(tempdir(), "rosling_test_final")
  if(dir.exists(test_path)) unlink(test_path, recursive = TRUE)
  
  info <- animint2dir(viz, out.dir = test_path, open.browser = FALSE)
  
  # check if point and rect geoms are present in the compiler metadata
  geom_names <- names(info$geoms)
  expect_true(any(grepl("point", geom_names)))
  expect_true(any(grepl("rect", geom_names)))
  
  # ensure the 'year' selector is working
  expect_true("year" %in% names(info$selectors))
 
  point_geom <- grep("point", geom_names, value = TRUE)[1]
  tsv_files <- list.files(test_path, pattern = paste0(point_geom, ".*\\.tsv$"), full.names = TRUE)
  
  expect_gt(length(tsv_files), 0)
  
  # check data integrity (10 subjects * 5 years = 50 rows)
  dat <- data.table::fread(tsv_files[1])
  expect_equal(nrow(dat), 50)
})