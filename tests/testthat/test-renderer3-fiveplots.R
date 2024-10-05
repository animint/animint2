
test_that("check rowspan, all plots in single row", {
  plot_of_3_dots_data <- data.frame(
    x = c(1, 2, 3), # x-coordinates of the dots
    y = c(1, 4, 9)  # y-coordinates of the dots
  )
  
  plot_of_3_dots <- ggplot(plot_of_3_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 3 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=0,col=2,rowspan=2)
  
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
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=0,col=0,rowspan=2)
  
  
  plot_of_4_dots_data <- data.frame(
    x = c(1, 2, 3,6), # x-coordinates of the dots
    y = c(1, 4, 9,13)  # y-coordinates of the dots
  )
  
  plot_of_4_dots <- ggplot(plot_of_4_dots_data, aes(x, y)) + 
    geom_point(size = 2) +            # Plot points with a specified size
    ggtitle("Plot of 4 Dots") +        # Add a title to the plot
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=0,col=3)
  
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
    xlab("X Axis") + ylab("Y Axis")+theme_animint(row=1,col=3)

  plot_list <- list(
    plot1=plot_of_1_dot,
    plot2=plot_of_2_dots,
    plot3=plot_of_3_dots,
    plot4=plot_of_4_dots,
    plot5=plot_of_5_dots,
    plot6=plot_of_6_dots
  )
  
  info <-animint2HTML(plot_list)
  
  tables <- getNodeSet(info$html, "//table[@style='display: inline-block;']")
  
  number_of_tables<-length(tables)
  
  expect_equal(number_of_tables,6)
})