library(animint2)

# sepal plot
p1 <- a_plot() + 
  a_geom_point(a_aes(Sepal.Length, Sepal.Width, colour = Species, size = Sepal.Width), 
             clickSelects = "Species", 
             data = iris) + 
  a_theme(panel.background = a_element_rect(fill = "lightblue"), 
        panel.border = a_element_rect(fill = NA, 
                                    color = "black", 
                                    size = 2, 
                                    linetype = "dashed"), 
        panel.margin = grid::unit(.1, "cm")) + 
  a_facet_wrap(~Species, nrow = 2)

# panel plot
p2 <- a_plot() + 
  a_geom_point(a_aes(Petal.Length, Petal.Width, colour = Species), 
             showSelected = "Species", data = iris) + 
  ggtitle("Petal Data") + 
  a_theme_bw()
viz <- list(sepal = p1, 
            petal = p2, 
            title = "Different Panel Styles", 
            selector.types=list(Species="multiple"))

animint2dir(viz, out.dir = "iris_animint")

animint2gist(viz, "Panel Backgrounds")
