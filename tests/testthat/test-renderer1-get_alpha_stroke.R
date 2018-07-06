acontext("get_alpha_stroke")

  data(state)
  states.data <- as.data.frame(state.x77)
  states.data$states <- rownames(states.data)
  colnames(states.data)[4] <- "Life.Exp"

  viz <- list(geoms_viz = ggplot(data = states.data,
                aes(x = Income, y = Life.Exp)) +
                geom_point(shape = 21,
                            colour = "black",
                            fill = "white",
                            # I include it here to modify the stroke of geom_point
                            # What do you think? Any advice? Is it a fraction the best way to set it?
                            alpha_stroke = 1/10,
                            validate_params = FALSE) +
                ggtitle("Income v. Life Expectancy") +
                theme(panel.background = element_rect(fill = NA),
                      panel.grid.major = element_line(colour = "grey90")
                ))

  info <- animint2HTML(viz)

test_that("alpha_stroke parameter is rendered as stroke-opacity style", {
  opacity.str <- getStyleValue(info$html, "//circle[@class='geom']", "stroke-opacity")
  opacity.num <- as.numeric(opacity.str)
  opacity.exp <- rep(1/10, nrow(states.data))
  expect_equal(opacity.num, opacity.exp)
})


test_that("alpha_stroke parameter is rendered as stroke-opacity style in rects", {
  viz <- list(segs = ggplot() +
                geom_rect(data = df, size = 0.01, color = "violet",
                          alpha_stroke = 1/10,
                          validate_params = FALSE,
                          aes(xmin = xmin, ymin = ymin,
                              xmax = xmax, ymax = ymax)))
  info <- animint2HTML(viz)

opacity.str <- getStyleValue(info$html, "//rect[@class='geom']", "stroke-opacity")
opacity.num <- as.numeric(opacity.str)
opacity.exp <- rep(1/10, nrow(states.data))
expect_equal(opacity.num, opacity.exp)
})


test_that("alpha_stroke parameter is rendered as stroke-opacity style in widerect", {
recommendation <- data.frame(
  min.C=21,
  max.C=23)
set.seed(1)
temp.time <- data.frame(
  time=strptime(paste0("2015-10-", 1:31), "%Y-%m-%d"),
  temp.C=rnorm(31))

viz <- list(
  gg=ggplot()+
    theme_bw()+
    theme_animint(height=200, width=2000)+
    geom_widerect(aes(ymin=min.C, ymax=max.C),
                  color=NA,
                  fill="grey",
                  data=recommendation,
                  alpha_stroke = 1/10,
                  validate_params = FALSE)+
    geom_line(aes(time, temp.C),
              data=temp.time)
  )

info <- animint2HTML(viz)

opacity.str <- getStyleValue(info$html, "//widerect[@class='geom_widerect']", "stroke-opacity")
opacity.num <- as.numeric(opacity.str)
opacity.exp <- rep(1/10, nrow(states.data))
expect_equal(opacity.num, opacity.exp)
})
