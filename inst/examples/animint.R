## Make a Gapminder plot (aka Google motion chart), which is actually
## just a scatterplot with size and color that moves over time.
library(animint2)
data(WorldBank)
gapminder <-
  list(title="Linked scatterplot and time series",
       ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, color=region),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       duration=list(year=1000),
       scatter=a_plot()+
       a_geom_point(a_aes(fertility.rate, life.expectancy,
                      key=country, colour=region, size=population),
                  showSelected="year",
                  clickSelects="country",
                  data=WorldBank)+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country),
                 showSelected=c("country", "year"),
                 data=WorldBank)+
       make_text(WorldBank, 5, 80, "year")+
       a_scale_size_animint(pixel.range=c(2,20), breaks=10^(4:9)))
animint2dir(gapminder, "WorldBank-viz")

data(worldPop)
## Linked bar and line plots of world population by subcontinent,
## inspired by polychartjs.
popPlots <-
  list(bars=a_plot()+
       a_geom_bar(a_aes(x=subcontinent, y=population),
                clickSelects="subcontinent",
                showSelected="year",
                data=worldPop, a_stat="identity", a_position="identity")+
       ## This make_text creates a a_geom_text that shows the current
       ## selected value of the year variable.
       make_text(worldPop, 1, 3e6, "year")+
       a_coord_flip(),
       lines=a_plot()+
       ## This make_tallrect tiles the background of the lineplot with
       ## rects that can be clicked to select the year variable.
       make_tallrect(worldPop, "year")+
       ## This a_geom_point does not have a_aes(clickSelects) so its alpha
       ## transparency behaves normally: all points have alpha=1/4.
       a_geom_point(a_aes(year, population, colour=type),
                  data=worldPop, size=4, alpha=1/4)+
       ## This a_geom_line DOES have a_aes(clickSelects) so only the
       ## selected line has the specified alpha=3/4. The other
       ## unselected lines have 0.5 less (alpha=1/4).
       a_geom_line(a_aes(year, population, group=subcontinent),
                 clickSelects="subcontinent",
                 data=worldPop, size=4, alpha=3/4))
animint2dir(popPlots, "WorldPop-interactive")
## Make it animated by specifying year as the variable to animate and
## an interval of 2000 milliseconds between animation frames.
popAnim <- c(popPlots, list(time=list(variable="year",ms=2000)))
animint2dir(popAnim, "WorldPop-animated")
## Make the animation smooth by specifying a duration of 1000 ms for
## geoms with a_aes(showSelected=year).
popSmooth <- c(popAnim, list(duration=list(year=1000)))
animint2dir(popSmooth, "WorldPop-smooth")

