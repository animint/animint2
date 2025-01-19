library(testthat)

test_that("Driver.js guided tour displays correct help text", {
  
  # Start RSelenium with Microsoft Edge (or any other browser)
  rD <- rsDriver(browser = "edge", port = 4444)
  remDr <- rD[["client"]]
  
  # Navigate to the HTML file
  remDr$navigate("file:///C:/Users/biplab sutradhar/OneDrive/Documents/WEB/exercisE/animint_tour/animint_vline_point_plot_output/index.html")
  
  # Click "Start Tour" button
  start_tour_button <- remDr$findElement(using = "id", "Start Tour")
  start_tour_button$clickElement()
  
  # Retrieve help text after the first click
  first_step_text <- remDr$findElement(using = "css", "#helpText")$getElementText()
  expected_first_text <- "Point Data are shown for the current selection of: year. Click to change selection of: country."
  expect_identical(first_step_text, expected_first_text)
  
  # Click "Next" button
  next_button <- remDr$findElement(using = "id", "Next")
  next_button$clickElement()
  
  # Retrieve help text after the second click
  second_step_text <- remDr$findElement(using = "css", "#helpText")$getElementText()
  expected_second_text <- "Vline Data are shown for the current selection of: year. Click to change selection of: country."
  expect_identical(second_step_text, expected_second_text)
  
  # Close the browser and stop the Selenium server
  remDr$close()
  rD[["server"]]$stop()
})
