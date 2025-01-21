

test_that("check rowspan", {
  plot_of_3_dots_data <- data.frame(
    x = c(1, 2, 3), # x-coordinates of the dots
    y = c(1, 4, 9)  # y-coordinates of the dots
  )
  
  plot_of_3_dots <- ggplot(plot_of_3_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 3 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=0,col=0,rowspan=2)
  
  plot_of_2_dots_data <- data.frame(
    x = c(1, 2), # x-coordinates of the dots
    y = c(1, 4)  # y-coordinates of the dots
  )
  
  plot_of_2_dots <- ggplot(plot_of_2_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 2 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=0,col=1)
  
  
  plot_of_1_dot_data <- data.frame(
    x = c(2), # x-coordinates of the dots
    y = c(2)  # y-coordinates of the dots
  )
  
  plot_of_1_dot <- ggplot(plot_of_1_dot_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 1 Dot") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=0,col=2,rowspan=2)
  
  car_weight_mpg <- ggplot(mtcars, aes(x = wt, y = mpg))+
    geom_point() +
    labs(title = "Car Weight vs. MPG") +
    theme(
      plot.title = element_text(color = "purple", size = 20, face = "bold"),
      panel.background = element_rect(fill = "lightyellow"),
      panel.grid.major = element_line(color = "gray", size = 0.8),
      axis.title.x = element_text(color = "blue", size = 14),
      axis.text.y = element_text(color = "red", size = 12),
      legend.position = "bottom",
      legend.background = element_rect(fill = "lightblue", color = "black", size = 0.5)
    )+theme_animint(row=0,col=3)
  
  plot_of_4_dots_data <- data.frame(
    x = c(1, 2, 3,6), # x-coordinates of the dots
    y = c(1, 4, 9,13)  # y-coordinates of the dots
  )
  
  plot_of_4_dots <- ggplot(plot_of_4_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 4 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=2,col=2)
  
  plot_of_4_dots_copy <- ggplot(plot_of_4_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 4 Dots Copy") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=2,col=3)
  
  
  plot_of_5_dots_data <- data.frame(
    x = c(1, 2, 3,6,9), # x-coordinates of the dots
    y = c(1, 4, 9,13,16)  # y-coordinates of the dots
  )
  
  plot_of_5_dots <- ggplot(plot_of_5_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 5 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=1,col=1)
  
  plot_of_6_dots_data <- data.frame(
    x = c(1, 2, 3,6,9,11), # x-coordinates of the dots
    y = c(1, 4, 9,13,16,18)  # y-coordinates of the dots
  )
  
  plot_of_6_dots <- ggplot(plot_of_6_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 6 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=2,col=0)
  
  
  plot_of_7_dots_data <- data.frame(
    x = c(1, 2, 3,6,9,11,12), # x-coordinates of the dots
    y = c(1, 4, 9,13,16,18,19)  # y-coordinates of the dots
  )
  
  plot_of_7_dots <- ggplot(plot_of_7_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 7 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=2,col=1)
  
  plot_of_8_dots_data <- data.frame(
    x = c(1, 2, 3,6,9,11,12,13), # x-coordinates of the dots
    y = c(1, 4, 9,13,16,18,19,21)  # y-coordinates of the dots
  )
  
  plot_of_8_dots <- ggplot(plot_of_8_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 8 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=1,col=3,rowspan=2)
  
  
  
  
  plot_list <- list(
    plot1=plot_of_3_dots,
    plot2=plot_of_2_dots,
    plot3=plot_of_1_dot,
    plot4=car_weight_mpg,
    plot5=plot_of_4_dots,
    plot6=plot_of_5_dots,
    plot7=plot_of_6_dots,
    plot8=plot_of_7_dots,
    plot9=plot_of_8_dots
  )
  
  info <-animint2HTML(plot_list)
  
  tables <- getNodeSet(info$html, "//table[@style='display: inline-block;']")
  
  number_of_tables<-length(tables)
  
  expect_equal(number_of_tables,9)
})