library(animint2)
library(XML)

set.seed(42)
nrow_val <- 10
ncol_val <- 10
size <- 15
nmax <- 20

population <- data.frame(
  x = rep(1:ncol_val, nrow_val),
  y = as.integer(gl(nrow_val, ncol_val)),
  id = 1:(nrow_val * ncol_val)
)

frames <- list()
for (i in 1:nmax) {
  sampled_ids <- sample(nrow_val * ncol_val, size)
  df <- population
  df$sampled <- ifelse(df$id %in% sampled_ids, "Sampled", "Population")
  df$iteration <- i
  frames[[i]] <- df
}

all_frames <- do.call(rbind, frames)
all_frames$iteration <- as.integer(all_frames$iteration)
all_frames$sampled <- as.character(all_frames$sampled)

grid_plot <- ggplot() +
  geom_point(
    data = all_frames,
    aes(x = x, y = y, color = sampled, size = sampled, key = id),
    showSelected = "iteration"
  ) +
  scale_color_manual(
    values = c("Population" = "steelblue", "Sampled" = "red")
  ) +
  scale_size_manual(
    values = c("Population" = 3, "Sampled" = 6)
  ) +
  ggtitle("Simple Random Sampling") +
  xlab("Column") + ylab("Row") +
  theme_animint(width = 500, height = 500)

viz_test <- animint(
  grid = grid_plot,
  time = list(variable = "iteration", ms = 800),
  title = "Simple Random Sampling Test"
)

test_that("circles render before and after clickID interaction", {

  res <- animint2HTML(viz_test)

  # --- BEFORE CLICK: verify initial render ---
  html_before <- getHTML()
  circles_before <- XML::getNodeSet(html_before, "//circle")
  expect_gte(length(circles_before), 100)

  # --- CLICK: advance to next iteration ---
  clickID("iteration_next")
  Sys.sleep(1)

  # --- AFTER CLICK: verify state updated ---
  html_after <- getHTML()
  circles_after <- XML::getNodeSet(html_after, "//circle")
  expect_gte(length(circles_after), 100)
})
