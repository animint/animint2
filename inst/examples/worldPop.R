library(animint2)
data(worldPop)
levs <- levels(worldPop$subcontinent)
subcont.means <- sapply(levs,function(l)mean(worldPop$pop[worldPop$sub==l]))
worldPop$subcontinent <- factor(worldPop$sub, levs[order(subcont.means)])
years <- unique(worldPop[,"year",drop=FALSE])
years$title <- factor(sprintf("Population in %d", years$year))
years$subcontinent <- factor(levels(worldPop$sub)[1])
years$population <- 3e6
## this should be similar to the example on polychartjs.com
popPlots <-
  list(bars=a_plot()+
         geom_bar(aes(x=subcontinent, y=population),
                  showSelected="year",
                  data=worldPop, a_stat="identity", position="identity")+
         geom_text(aes(x=subcontinent, y=population,
                       label=title),
                   showSelected="year",
                   data=years) +
         a_coord_flip(),
       lines=a_plot()+
         geom_vline(aes(xintercept=year),
                    clickSelects="year",
                    data=years, alpha=1/2, size=12)+
         geom_line(aes(year, population, group=subcontinent),
                   data=worldPop, alpha=3/4, size=4)+
         geom_point(aes(year, population, fill=type, colour=type),
                    data=worldPop))
info <- animint2dir(popPlots, "worldPop")

## we should at least see the bars in this simpler test.
onebar <- a_plot()+
  geom_bar(aes(subcontinent), data=worldPop)
animint2dir(list(bar=onebar))

## Population barplots broken down by year.
library(grid)
popPlots$bars+
  a_facet_wrap("year")+
  a_theme_bw()+
  a_theme(panel.margin=unit(0,"cm"))

## simpler example using make_tallrect.
data(worldPop)
popPlot <- a_plot()+
  make_tallrect(worldPop, "year")+
  geom_line(aes(year, population, group=subcontinent),
            data=worldPop, size=4)
print(popPlot)

animint2dir(list(lines=popPlot,bars=popPlots$bars))

## Show the currently selected continent on both plots.
popPlots2 <-
  list(bars=a_plot()+
         geom_bar(aes(x=subcontinent, y=population),
                  showSelected="year", clickSelects="subcontinent",
                  data=worldPop, a_stat="identity", position="identity")+
         geom_text(aes(x=subcontinent, y=population,
                       label=title),
                   showSelected="year",
                   data=years) +
         a_coord_flip(),
       lines=a_plot()+
         make_tallrect(worldPop, "year")+
         geom_point(aes(year, population, colour=type),
                    data=worldPop, size=4, alpha=1/4)+
         ##a_scale_colour_manual(values=c("black", "red"))+
         geom_line(aes(year, population, group=subcontinent),
                   clickSelects="subcontinent",
                   data=worldPop, size=4, alpha=3/4))
animint2dir(popPlots2)


library(plyr)
popCumSum <- ddply(worldPop[order(worldPop$year, worldPop$subcontinent),], .(year), transform, 
                   cumPop = cumsum(population)/sum(population), 
                   cumPop.lower = cumsum(c(0, population[-length(population)]))/sum(population))
popCumSum$cumCenter = rowMeans(popCumSum[,c("cumPop", "cumPop.lower")])
popCumSum$subcontinent.names <- factor(as.character(popCumSum$subcontinent)) # alphabetize
popCumSum$subcontinent.lab.height <- 1-as.numeric(popCumSum$subcontinent.names)/15

popPlots3 <-
  list(bars=a_plot()+
         geom_bar(aes(x=subcontinent, y=population),
                  showSelected="year", clickSelects="subcontinent",
                  data=worldPop, a_stat="identity", position="identity")+
         geom_text(aes(x=subcontinent, y=population,
                       label=title),
                   showSelected="year",
                   data=years) +
         a_coord_flip(),
       lines=a_plot()+
         make_tallrect(worldPop, "year")+
         geom_line(aes(year, population, group=subcontinent),
                   clickSelects="subcontinent",
                   data=worldPop, size=4, alpha=3/4)+
         geom_point(aes(year, population, colour=type), 
                    clickSelects="subcontinent",
                    data=worldPop, size=4, alpha=.6)+
         a_scale_colour_manual(values=c("black", "red")),
       stack=a_plot()+ 
         geom_rect(aes(xmin=0, xmax=0.4, ymin=cumPop.lower, ymax=cumPop, fill=factor(subcontinent)), 
                   showSelected="year", clickSelects="subcontinent",
                   data=popCumSum, colour="#000000")+
         a_scale_y_continuous(limits=c(0,1), breaks=c(0, 1), labels=NULL) + 
         a_scale_x_continuous(labels=NULL) + 
         a_scale_fill_discrete("Subcontinent") +
         xlab("") + ylab(""),
      width=list(bars = 400, lines = 400, stack = 200), height=list(400)
  )
animint2dir(popPlots3)
## TODO: separate bar stacks for different divisions: What's there replicates polycharts.js, 
## but it's not correct (i.e. N. America and The Americas in the same stack). 

## TODO: figure out how to sort factor order by population for bars?
