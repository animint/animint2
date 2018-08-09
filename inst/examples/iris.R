library(animint2)
iris$id <- 1:nrow(iris)
viz <-
  list(petal=a_plot()+
       a_geom_point(a_aes(Petal.Width, Petal.Length, fill=Species),
                  clickSelects="id",
                  data=iris),
       sepal=a_plot()+
       a_geom_point(a_aes(Sepal.Width, Sepal.Length, fill=Species),
                  clickSelects="id",
                  data=iris))
animint2dir(viz, "iris")
