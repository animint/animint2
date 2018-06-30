library(animint2)
iris$id <- 1:nrow(iris)
viz <-
  list(petal=a_plot()+
       geom_point(aes(Petal.Width, Petal.Length, fill=Species),
                  clickSelects="id",
                  data=iris),
       sepal=a_plot()+
       geom_point(aes(Sepal.Width, Sepal.Length, fill=Species),
                  clickSelects="id",
                  data=iris))
animint2dir(viz, "iris")
