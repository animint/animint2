library(animint2)
gg <- ggplot(faithfuld, aes(x = waiting, y = eruptions, z = density)) +
    geom_contour()
print(gg)
if(requireNamespace("ggplot2")){
    print(gg)
}
