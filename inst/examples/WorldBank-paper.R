library(animint2)
data(WorldBank)
WorldBank$region <-
  sub(" (all income levels)", "", WorldBank$region, fixed=TRUE)
wb.paper <-
  list(ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_guides(color="none")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 showSelected="region",
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),
       scatter=a_plot()+
       a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=WorldBank)+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                     key=country), #also use key here!
                 clickSelects="country",
                 showSelected=c("country", "year", "region"),
                 data=WorldBank)+
       a_scale_size_animint(breaks=10^(9:5))+
       make_text(WorldBank, 5, 80, "year"),
       time=list(variable="year",ms=3000),
       duration=list(year=1000),
       selector.types=list(country="multiple", region="multiple"),
       first=list(
         year=1979,
         country=c("United States", "Vietnam"),
         region=c("East Asia & Pacific", "North America")
         ),
       title="World Bank data (Animint paper version)")
animint2dir(wb.paper, "WorldBank-paper")

