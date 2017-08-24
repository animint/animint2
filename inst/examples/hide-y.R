library(animint2)
data(worldPop)

## Linked bar and line plots of world population by subcontinent,
## inspired by polychartjs.

popPlots <-list(bars=ggplot()
                +geom_bar(aes(x=subcontinent, y=population,
                              fill=subcontinent,alpha=.5),
                          clickSelects="subcontinent",
                          data=worldPop, stat="identity",
                          position="identity")
                +coord_flip(),

lines=ggplot()+ geom_point(aes(year, population, colour=type),
                           data=worldPop, size=4, alpha=1/4)+
  geom_line(aes(year, population, group=subcontinent),
            clickSelects="subcontinent", data=worldPop, size=4, alpha=3/4))

animint2dir(popPlots)

hide.y <- ggplot()+
  geom_bar(aes(x=subcontinent, y=population, fill=subcontinent),
           clickSelects="subcontinent",
           data=worldPop, 
           stat="identity", position="identity")+
  coord_flip()+
  theme(axis.line.y=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), axis.title.y=element_blank())
print(hide.y)
pop.no.y <- popPlots
pop.no.y$bars <- hide.y
animint2dir(pop.no.y, "hide-y")
