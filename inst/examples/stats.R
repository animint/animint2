library(animint2)

my.iris <- iris
my.iris$id <- 1:nrow(my.iris)
my.iris$individuals <- 1

viz <-
  list(scatter=a_plot()+
       a_geom_point(a_aes(Sepal.Length, Petal.Length),
                  clickSelects="id",
                  data=my.iris, alpha=6/10),

       selector.types=list(id="multiple"),

       counts=a_plot()+
       a_geom_point(a_aes(Species, individuals),
                  showSelected="id",
### TODO: compute this a_stat in the renderer, to see the nummber of
### currently selected points in each of the 3 classes.
                  a_stat="summary",
                  fun.y="sum",
                  data=my.iris))

animint2dir(viz)
