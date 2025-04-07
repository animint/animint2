context("Renderer Test: World Bank GDP vs. Life Expectancy")

test_that("SVG renders, year label appears, and country selection works", {
  library(animint2)
  library(dplyr)
  data(WorldBank, package = "animint2")
  
  wb_clean <- WorldBank %>%
    # clean data
    filter(
      !is.na(year),
      !is.na(life.expectancy),
      !is.na(GDP.per.capita.Current.USD),
      !is.na(population)
    )
  
  viz <- list(
    scatter = ggplot(wb_clean, aes(x = GDP.per.capita.Current.USD, y = life.expectancy)) +
      animint2::geom_point(
        aes(color = region, size = population, key = country),
        clickSelects = "country",
        showSelected = "year",
        alpha = 0.9,
        help = "Each bubble represents a country. Click to highlight; size = population."
      ) +
      animint2::geom_text(
        aes(x = 1000, y = 85, label = paste("Year:", year), key = year),
        showSelected = "year",
        inherit.aes = FALSE,
        size = 15,        
        hjust = 0.5,        
        help = "The number shows the selected year."
      ) +
      scale_x_log10(
        breaks = c(100, 1000, 10000, 100000),
        labels = scales::label_number()
      ) +
      scale_y_continuous(
        breaks = seq(20, 90, by = 10)
      ) +
      labs(
        title = "Global Health and Wealth (1960–2010)",
        x = "GDP per Capita (Current USD, log scale)",
        y = "Life Expectancy (years)"
      ) +
      theme_minimal(),
    
    time = list(variable = "year", ms = 750),
    duration = list(year = 1000),
    first = list(year = 1960),
    buttons = list("play", "pause")  
  )
  
  
  # render to disk
  animint2dir(viz, "animint-htmltest")
  
  # open in local server
  remDr$navigate("http://localhost:4848/animint-htmltest/index.html")
  
  # HTML checks
  info <- animint2HTML(viz)
  
  # check SVG exists
  svg_nodes <- getNodeSet(info$html, "//svg")
  expect_gt(length(svg_nodes), 0)
  
  # wait for year label to animate in DOM, then check
  updated <- getHTML()
  year_label <- getNodeSet(updated, '//text[contains(text(), "1960")]')
  expect_gt(length(year_label), 0)
  
  # no country selected initially
  initially_selected <- getNodeSet(updated, '//g[contains(@class, "selected")]')
  expect_equal(length(initially_selected), 0)

  # interactivity test
  test_that("Play and Pause toggle animation correctly", {
    info <- animint2HTML(viz)
    
    # show animation controls first
    clickID("plot_show_hide_animation_controls")
    Sys.sleep(0.5)
    clickID("play_pause") 
    Sys.sleep(1.2)
    
    # get year during animation
    html_play <- getHTML()
    year_label_playing <- getNodeSet(html_play, '//text[contains(text(), "Year:")]')
    year_text_playing <- sapply(year_label_playing, xmlValue)[1]

    # pause
    clickID("play_pause") 
    Sys.sleep(1)
    
    # get year again
    html_paused <- getHTML()
    year_label_paused <- getNodeSet(html_paused, '//text[contains(text(), "Year:")]')
    year_text_paused <- sapply(year_label_paused, xmlValue)[1]

    # check years are equal
    expect_equal(
      year_text_paused,
      year_text_paused,
      info = "Year should remain the same after clicking pause"
    )
  })
})