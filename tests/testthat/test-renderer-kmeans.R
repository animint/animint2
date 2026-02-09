# Ensure only animint2 is handled
if("package:ggplot2" %in% search()) detach("package:ggplot2", unload=TRUE)
library(animint2)
library(data.table)

# 1. DATA GENERATION (Self-contained)
set.seed(1)
points <- data.table(x = c(rnorm(50, 0), rnorm(50, 3)), y = c(rnorm(50, 0), rnorm(50, 3)))
k <- 2
centers_mat <- as.matrix(points[sample(.N, k), .(x, y)])
max_iter <- 6
points_list <- list(); centers_list <- list(); objective_list <- list()

for (iter in 1:max_iter) {
  dist_mat <- as.matrix(dist(rbind(points[, .(x, y)], centers_mat)))
  dist_mat <- dist_mat[1:nrow(points), (nrow(points)+1):(nrow(points)+k)]
  cluster <- apply(dist_mat, 1, which.min)
  points_iter <- copy(points)[, `:=`(cluster = factor(cluster), iteration = iter)]
  centers_iter_df <- points_iter[, .(x = mean(x), y = mean(y)), by = cluster][, iteration := iter]
  obj <- sum((points_iter$x - centers_iter_df$x[as.integer(cluster)])^2 + (points_iter$y - centers_iter_df$y[as.integer(cluster)])^2)
  objective_list[[iter]] <- data.table(iteration = iter, value = obj)
  points_list[[iter]]  <- points_iter
  centers_list[[iter]] <- centers_iter_df
  centers_mat <- as.matrix(centers_iter_df[, .(x, y)])
}

p_df <- rbindlist(points_list)
c_df <- rbindlist(centers_list)
o_df <- rbindlist(objective_list)

# 2. THE VIZ (Using explicit animint2 calls to be 100% safe)
# 

viz <- list(
  scatter = animint2::ggplot() +
    animint2::theme_bw() +
    animint2::geom_point(aes(x=x, y=y, color=cluster), 
                         data=p_df, 
                         showSelected="iteration", 
                         size=3) +
    animint2::geom_point(aes(x=x, y=y), 
                         data=c_df, 
                         showSelected="iteration", 
                         shape=21, size=7, fill="white", stroke=2) +
    animint2::scale_color_manual(values = c("1" = "#FF4B2B", "2" = "#1CB5E0")),
  
  objective = animint2::ggplot() +
    animint2::theme_bw() +
    animint2::geom_line(aes(x=iteration, y=value), data=o_df) +
    animint2::geom_point(aes(x=iteration, y=value), data=o_df, clickSelects="iteration", size=5) +
    animint2::ggtitle("Click points to select Iteration"),
  
  time = list(variable = "iteration", ms = 2000),
  duration = list(iteration = 1000)
)

# 3. RENDER
test_dir <- file.path(tempdir(), "gsoc_hard_test")
animint2::animint2dir(viz, out.dir = test_dir, open.browser = FALSE)

# 4. START SERVER
servr::httd(test_dir)