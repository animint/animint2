library(testthat)
library(animint2)
library(data.table)

context("Rosling Bubbles Hard Test")

# data prep
set.seed(42)
n_individuals <- 10
n_years <- 5

# create main data
rosling_data <- rbindlist(lapply(1:n_years, function(k) {
  data.table(
    id = factor(paste("Subject", 1:n_individuals)),
    year = as.numeric(k), # <--- Numeric
    x = rnorm(n_individuals, mean = k/5, sd = 2), 
    y = rnorm(n_individuals, mean = k/10, sd = 2),
    size = abs(rnorm(n_individuals, 2, 0.5))
  )
}))

# create label data
year_labels <- data.table(
  year = as.numeric(1:n_years), # <--- Explicitly Numeric to match
  year_text = as.character(1:n_years), 
  key_id = 1
)

#viz-definition
viz <- list(
  bubbles = ggplot() +
    geom_text(data = year_labels,
              aes(x = 5, y = 2.5, label = year_text, key = key_id),
              showSelected = "year", 
              size = 60, color = "grey92") +
    geom_point(data = rosling_data,
               aes(x = x, y = y, size = size, fill = id, key = id),
               showSelected = "year", 
               clickSelects = "id",   
               shape = 21, color = "black", alpha = 0.7) +
    theme_bw() +
    ggtitle("Bubbles Plot"),
  
  rects = ggplot() +
    geom_rect(data = rosling_data,
              aes(xmin = x - size/2, xmax = x + size/2,
                  ymin = y - size/2, ymax = y + size/2,
                  fill = id, key = id),
              showSelected = "year", 
              clickSelects = "id",   
              color = "black", alpha = 0.7) +
    theme_bw() + 
    ggtitle("Rectangles Plot"),
  
  time = list(variable = "year", ms = 1000),
  duration = list(year = 1000)
)

#test-case
test_that("compiler correctly generates tsv chunks for point and rect layers", {
  
  test_path <- file.path(tempdir(), paste0("test_", as.numeric(Sys.time())))
  
  # Run compiler
  info <- animint2dir(viz, out.dir = test_path, open.browser = FALSE)
  
  # Check 1: Geoms identified
  geom_names <- names(info$geoms)
  expect_true(any(grepl("point", geom_names)))
  expect_true(any(grepl("rect", geom_names)))
  
  # Check 2: Selector registered
  expect_true("year" %in% names(info$selectors))
  
  # Check 3: TSV Files exist
  all_tsvs <- list.files(test_path, pattern = ".*\\.tsv$")
  expect_gt(length(all_tsvs), 0)
  
  # Check 4: Data row count (50)
  point_geom <- grep("point", geom_names, value = TRUE)[1]
  point_tsv <- list.files(test_path, pattern = paste0(point_geom, ".*\\.tsv$"), full.names = TRUE)
  dat <- data.table::fread(point_tsv[1])
  expect_equal(nrow(dat), 50)
  
  message(" SUCCESS! ")
})