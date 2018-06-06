acontext("get_alpha_stroke")

test_that("set get_alpha_stroke in a viz", {

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
                            get_alpha_stroke = 1/10,
                            validate_params = FALSE) +
                ggtitle("Income v. Life Expectancy") +
                theme(panel.background = element_rect(fill = NA),
                      panel.grid.major = element_line(colour = "grey90")
                )

})

# I am also developing a cartogram with ggplot2 to test get_aplha_stroke in the border of the states
# I think it could be interesting as there are not tests or examples with maps. We can reuse it later for the new documentation
# It should be ready tomorrow

test_that("set get_alpha_stroke in a viz", {

# Not sure if importing tidyverse and viridis is going to cause any conflict with animint2

library(tidyverse)
library(viridis)

usa <- tribble(
    ~state, ~code, ~x, ~y,
    "Hawai", "HI", 1, 1,
    "Alaska", "AK", 1, 8,
    "California", "CA", 2, 4,
    "Oregon", "OR", 2, 5,
    "Washington", "WA", 2, 6,
    "Arizona", "AZ", 3, 3,
    "Utah", "UT", 3, 4,
    "Nevada", "NV", 3, 5,
    "Idaho", "ID", 3, 6,
    "New Mexico", "NM", 4, 3,
    "Colorado", "CO", 4, 4,
    "Wyoming", "WY", 4, 5,
    "Montana", "MN", 4, 6,
    "Texas", "TX", 5, 1,
    "Oklahoma", "OK", 5, 2,
    "Kansas", "KS", 5, 3,
    "Nebraska", "NE", 5, 4,
    "South Dakota", "SD", 5, 5,
    "North Dakota", "ND", 5, 6,
    "Louisiana", "LA", 6, 2,
    "Arkansas", "AR", 6, 3,
    "Montana", "MT", 6, 4,
    "Iowa", "IA", 6, 5,
    "Minnesota", "MN", 6, 6,
    "Mississippi", "MS", 7, 2,
    "Tennessee", "TN", 7, 3,
    "Kentucky", "KY", 7, 4,
    "Indiana", "IN", 7, 5,
    "Illinois", "IL", 7, 6,
    "Alabama", "AL", 8, 2,
    "North Carolina", "NC", 8, 3,
    "West Virginia", "WV", 8, 4,
    "Ohio", "OH", 8, 5,
    "Wisconsin", "WI", 8, 6,
    "Georgia", "GA", 9, 2,
    "South Carolina", "SC", 9, 3,
    "Virginia", "VA", 9, 4,
    "Pennsylvania", "PA", 9, 5,
    "Michigan", "MI", 9, 6,
    "Florida", "FL", 10, 1,
    "District of Columbia", "DC", 10, 3,
    "Maryland", "MD", 10, 4,
    "New Jersey", "NJ", 10, 5,
    "New York", "NY", 10, 6,
    "Delaware", "DE", 11, 4,
    "Connecticut", "CT", 11, 5,
    "Rhode Island", "RI", 11, 6,
    "Vermont", "VT", 11, 7,
    "Massachusetts", "MA", 12, 6,
    "New Hampshire", "NH", 12, 7,
    "Maine", "ME", 12, 8,
)

df <- tibble(
    code = c("DE", "NY", "AL", "FL", "NJ", "VT"),
    `random_data` = c(8, 10, 1, 6, 7, 4)
)

usa %>%
    left_join(df, by = "code") %>%
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = `random_data`),
                  get_alpha_stroke = 1/10,
                  validate_params = FALSE) +
    geom_text(aes(label = code), color = "white") +
    coord_fixed(ratio = 1) +
    theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
    scale_fill_viridis(na.value = "#E1E1E1", option = "magma", begin = 0.1, end = 0.9)


})
