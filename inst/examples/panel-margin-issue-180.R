# Issue #180 / PR #286 — horizontal facet spacing (panel.margin)
# Run from repo root: source("inst/examples/panel-margin-issue-180.R")
library(animint2)

make_plot <- function(n) {
  list(
    plot1 = ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
      geom_point() +
      facet_grid(. ~ Species) +
      theme_bw() +
      theme(panel.margin = grid::unit(n, "lines"))
  )
}

# Screenshot 1: panel.margin = 0 lines (tight columns)
animint2dir(make_plot(0), "issue180_horizontal_margin0", open.browser = TRUE)

# Screenshot 2: panel.margin = 2 lines (wider gaps between columns)
animint2dir(make_plot(2), "issue180_horizontal_margin2", open.browser = TRUE)
