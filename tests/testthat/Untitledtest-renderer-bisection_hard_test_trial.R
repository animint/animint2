library(ggplot2)
library(animint2)

# Make iteration a factor
bisection_df <- data.frame(
  x = seq(-2, 2, length.out = 100),
  y = (seq(-2, 2, length.out = 100))^3 - seq(-2, 2, length.out = 100),
  iteration = factor(rep(1:5, each = 20))
)

interval_df <- data.frame(
  m = c(-1, -0.5, 0, 0.5, 1),
  iteration = factor(1:5)
)

# Create the animint2 object using showSelected as a parameter
bisection_animint2 <- animint(
  plot1 = ggplot(bisection_df, aes(x = x, y = y)) +
    geom_line(showSelected = "iteration") +  # parameter, not inside aes()
    geom_segment(
      data = interval_df,
      aes(x = m, xend = m, y = -Inf, yend = Inf),
      showSelected = "iteration",            # parameter here
      color = "red"
    ) +
    labs(title = "Bisection Method Visualization", x = "x", y = "f(x)"),
  
  # Animation settings
  time = list(variable = "iteration", ms = 2000)
)

# Render
animint2dir(
  bisection_animint2,
  out.dir = "~/Desktop/animint-gallery/bisection_animint2_render",
  open.browser = TRUE
)