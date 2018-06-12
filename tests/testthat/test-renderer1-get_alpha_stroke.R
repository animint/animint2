acontext("get_alpha_stroke")

  data(state)
  states.data <- as.data.frame(state.x77)
  states.data$states <- rownames(states.data)
  colnames(states.data)[4] <- "Life.Exp"

  viz <- ggplot(data = states.data,
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
                )


test_that("alpha_stroke parameter is rendered as stroke-opacity style", {
  opacity.str <- getStyleValue(info$html, "//circle[@class='geom']", "stroke-opacity")
  opacity.num <- as.numeric(opacity.str)
  opacity.exp <- rep(1/10, nrow(states.data))
  expect_equal(opacity.num, opacity.exp)
})
