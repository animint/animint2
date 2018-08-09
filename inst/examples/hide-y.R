library(animint2)
data(worldPop)

## Linked bar and line plots of world population by subcontinent,
## inspired by polychartjs.

popPlots <-list(bars=a_plot()
                +a_geom_bar(a_aes(x=subcontinent, y=population,
                              fill=subcontinent,alpha=.5),
                          clickSelects="subcontinent",
                          data=worldPop, a_stat="identity",
                          a_position="identity")+ a_coord_flip(),

lines=a_plot()+ a_geom_point(a_aes(year, population, colour=type),
                           data=worldPop, size=4, alpha=1/4)+
  a_geom_line(a_aes(year, population, group=subcontinent),
            clickSelects="subcontinent", data=worldPop, size=4, alpha=3/4))

animint2dir(popPlots)

hide.y <- a_plot()+
  a_geom_bar(a_aes(x=subcontinent, y=population, fill=subcontinent),
           clickSelects="subcontinent",
           data=worldPop, 
           a_stat="identity", a_position="identity")+ a_coord_flip()+
  a_theme(axis.line.y=a_element_blank(), axis.text.y=a_element_blank(), 
        axis.ticks.y=a_element_blank(), axis.title.y=a_element_blank())
print(hide.y)
pop.no.y <- popPlots
pop.no.y$bars <- hide.y
animint2dir(pop.no.y, "hide-y")
