library(animint2)
time <- 1:20
count <- 3:22 + rnorm(20)
count_all <- 201:220 + rnorm(20)
name <- rep("张三", 20)

data <- cbind(time, count, count_all, name)

time <- 1:20
count <- 6:25 + rnorm(20)
count_all <- 300:319 + rnorm(20)
name <- rep("李四", 20)

data <- rbind( data, cbind(time, count, count_all, name) )
data <- data.frame(data)
data$time <- as.numeric(data$time)
data$count <- as.numeric(data$count)
data$count_all <- as.numeric(data$count_all)

viz <-
list(

ts = a_plot() +
  make_tallrect( data, "time" ) +
  a_geom_line(a_aes(time, count, group = name, colour = name),
             clickSelects = "name",
             data = data, size = 3, alpha = 0.8 ),

time = list( variable="time",ms = 3000 ),

duration = list( time = 1000 ),

scatter = a_plot()+
  a_geom_point( a_aes(count_all, count, colour = name, size = count_all),
              clickSelects = "name",
              showSelected = "time",
              data = data ) +
  a_geom_text( a_aes(time, count, a_label = name),
             showSelected = c("name", "time"),
             data=data ) +
  ##make_text( data, 80, 90, "time" ) +
  continuous_a_scale("size","area",palette = function(x){
    scales:::rescale(sqrt(abs(x)), c(1,15), c(0,1))
  }) +
  xlim(0, 50)

)

animint2dir( viz , "chinese")
system("cat chinese/a_geom4_text_scatter_chunk1.tsv")

animint2gist(viz)
