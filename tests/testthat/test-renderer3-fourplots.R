

test_that("check if two plots exist", {
  data1 <- data.frame(
    x = c(1, 2, 3), # x-coordinates of the dots
    y = c(1, 4, 9)  # y-coordinates of the dots
  )
  
  plot1 <- ggplot(data1, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 3 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme(row=2,col=0)
  #theme(plot.title = element_text(color = "purple", size = 20, face = "bold"))
  data2 <- data.frame(
    x = c(1, 2), # x-coordinates of the dots
    y = c(1, 4)  # y-coordinates of the dots
  )
  
  plot2 <- ggplot(data2, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 2 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme(row=2,col=2)
  
  data3 <- data.frame(
    x = c(2), # x-coordinates of the dots
    y = c(2)  # y-coordinates of the dots
  )
  
  plot3 <- ggplot(data3, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 1 Dot") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme(row=0)
  plot4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(title = "Car Weight vs. MPG") +
    theme(
      plot.title = element_text(color = "purple", size = 20, face = "bold"),
      panel.background = element_rect(fill = "lightyellow"),
      panel.grid.major = element_line(color = "gray", size = 0.8),
      axis.title.x = element_text(color = "blue", size = 14),
      axis.text.y = element_text(color = "red", size = 12),
      legend.position = "bottom",
      legend.background = element_rect(fill = "lightblue", color = "black", size = 0.5),
      row=1,
      col=1
    )
  
  plot_list <- list(
    plot1=plot1,
    plot2=plot2,
    plot3=plot3,
    plot4=plot4
  )
  
  info <-animint2HTML(plot_list)
  titles <- getNodeSet(info$html, "//text[@class='plottitle']")
  
  points <- getNodeSet(info$html, "//circle[@class='geom']")
  
  number_of_points <- length(points)
  tables <- getNodeSet(info$html, "//table[@style='display: inline-block;']")
  svgs <- getNodeSet(info$html, "//svg[contains(@id,'')]")
  number_of_tables<-length(tables)
  
  number_of_points <- length(points)
  svg_dimensions <- lapply(svgs, function(svg) {
    list(
      height = as.numeric(xmlGetAttr(svg, "height")),
      width = as.numeric(xmlGetAttr(svg, "width"))
    )
  })
  for (dim in svg_dimensions) {
    expect_equal(dim$height, 400)
    expect_equal(dim$width, 400)
    
  }
  
  #expect_match(xmlValue(titles[[1]]), "Plot of 3 Dots")
  #expect_match(xmlValue(titles[[2]]), "Plot of 2 Dots")
  #expect_match(xmlValue(titles[[3]]), "Plot of 1 Dot")
  #expect_equal(number_of_points,6)
  #expect_equal(number_of_tables,3)
})
