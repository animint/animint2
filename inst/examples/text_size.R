library(animint2)
data(WorldBank)

wb  <- WorldBank[WorldBank$year == 2010,]

p <- a_plot()+
  a_geom_text(a_aes(y=fertility.rate, x=life.expectancy,
                a_label=iso2c, size=population, colour=population),
            data=wb)+
  a_scale_size_continuous(range=c(10,20))
print(p)
animint2dir(list(plot1=p), "text_size")

