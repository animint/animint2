works_with_R("3.0.2", ggplot2="0.9.3.1", maps="2.3.6", animint="2014.1.27")

data(canada.cities)
canada.cities$name <- with(canada.cities, reorder(name, -pop))
viz <-
    list(map=ggplot()+
         borders(regions="canada")+
         coord_equal()+
         make_text(canada.cities, -125, 80, "name")+
         geom_point(aes(long, lat), showSelected="name",
                    data=canada.cities, colour="red", size=4),
         bar=ggplot()+
         geom_bar(aes(name, log10(pop)), clickSelects="name",
                  data=canada.cities, stat="identity")+
         coord_flip(),
         height=list(bar=8000))
animint2dir(viz, "canada-cities")
## I would have liked the next plot to cycle through cities, but there
## is a bug in the current animint code, I guess it has something to
## do with non-numeric time variables.
## 2017-08-24: TODO: Does this bug still exist???
bug <- c(viz, list(time=list(variable="name", ms=2000)))
animint2dir(bug)
