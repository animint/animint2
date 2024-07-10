

test_that("check if two plots exist", {
  data1 <- data.frame(
    x = c(1, 2, 3), # x-coordinates of the dots
    y = c(1, 4, 9)  # y-coordinates of the dots
  )
  
  plot1 <- ggplot(data1, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 3 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")
  
  data2 <- data.frame(
    x = c(1, 2), # x-coordinates of the dots
    y = c(1, 4)  # y-coordinates of the dots
  )
  
  plot2 <- ggplot(data2, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 2 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")
  
  data3 <- data.frame(
    x = c(2), # x-coordinates of the dots
    y = c(2)  # y-coordinates of the dots
  )
  
  plot3 <- ggplot(data3, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 1 Dot") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")
  
  plot_list <- list(
    plot1 = plot1,
    plot2=plot2,
    plot3=plot3
  )
  
  info <-animint2HTML(plot_list)
  titles <- getNodeSet(info$html, "//text[@class='plottitle']")
  expect_match(xmlValue(titles[[1]]), "Plot of 3 Dots")
  expect_match(xmlValue(titles[[2]]), "Plot of 2 Dots")
  expect_match(xmlValue(titles[[3]]), "Plot of 1 Dot")
})
