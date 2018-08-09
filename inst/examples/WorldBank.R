library(animint2)
data(WorldBank)

wb.all <-
  list(scatter=a_plot()+
       a_geom_point(a_aes(life.expectancy, fertility.rate, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=WorldBank)+
       a_geom_text(a_aes(life.expectancy, fertility.rate, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year"),
                 data=WorldBank)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=a_plot()+
       a_theme_animint(height=2400)+
       a_geom_bar(a_aes(country, life.expectancy, fill=region),
                showSelected="year", clickSelects="country",
                data=WorldBank, a_stat="identity", a_position="identity")+
       a_coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country="United States"),
       title="World Bank data (single selection)")
animint2dir(wb.all, "WorldBank-all")

## M <- gvisMotionChart(WorldBank,
##                      idvar="country", timevar="year",
##                      xvar="life.expectancy", yvar="fertility.rate",
##                      colorvar="region", sizevar="population",
##                      options=list(width=700, height=600))

## This example is BAD because it does not specify a key variable, so
## the animated transitions result in points flying over the
## scatterplot, which is not good for understand the evolution over
## time of these countries' data.
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
not.na[not.na$country=="Kuwait", "population"] <- 1700000
bad <-
  list(scatter=a_plot()+
       a_geom_point(a_aes(life.expectancy, fertility.rate, colour=region, size=population),
                  clickSelects="country",
                  showSelected="year",
                  data=not.na)+
       a_geom_text(a_aes(life.expectancy, fertility.rate, a_label=country),
                 showSelected=c("country", "year"),
                 data=not.na)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=a_plot()+
       a_theme_animint(height=2400)+
       a_geom_bar(a_aes(country, life.expectancy, fill=region),
                showSelected="year", clickSelects="country",
                data=WorldBank, a_stat="identity", a_position="identity")+
       a_coord_flip(),
       duration=list(year=1000))
animint2dir(bad, "WorldBank-bad")

## This example is good because it uses constancy
## http://bost.ocks.org/mike/constancy/
good <-
  list(scatter=a_plot()+
       a_geom_point(a_aes(life.expectancy, fertility.rate, colour=region, size=population,
                      tooltip=paste(country, "population", population),
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=not.na)+
       a_geom_text(a_aes(life.expectancy, fertility.rate, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year"),
                 data=not.na)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(WorldBank, 55, 9, "year"),
       ts=a_plot()+
       make_tallrect(WorldBank, "year")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       bar=a_plot()+
       a_theme_animint(height=2400)+
       a_geom_bar(a_aes(country, life.expectancy, fill=region,
                    key=country),
                showSelected="year", clickSelects="country",
                data=WorldBank, a_stat="identity", a_position="identity")+
       a_coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country="United States"),
       title="World Bank data (single selection)")
animint2dir(good, "WorldBank-good")

## This example additionally uses multiple selection on countries.
library(dplyr)
max.years <- not.na %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  mutate(year=2012)
wb.mult <-
  list(ts=a_plot()+
       make_tallrect(not.na, "year")+
       a_theme_animint(width=500)+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=not.na, size=4, alpha=3/5)+
       a_geom_point(a_aes(year, life.expectancy, color=region),
                  showSelected="country", clickSelects="country",
                  data=not.na)+
       a_scale_x_continuous(limits=c(1960, 2030), breaks=seq(1960, 2010, by=10))+
       a_geom_text(a_aes(year, life.expectancy, colour=region, a_label=country),
                 showSelected="country",
                 clickSelects="country",
                 data=max.years, hjust=0),
       scatter=a_plot()+
       a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=not.na)+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year"),
                 clickSelects="country",
                 data=not.na)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(not.na, 5, 85, "year"),
       
       time=list(variable="year",ms=3000),
       
       duration=list(year=1000),
       
       first=list(year=1975, country=c("United States", "Vietnam")),
       
       selector.types=list(country="multiple"),
       
       title="World Bank data (multiple selection)")
animint2dir(wb.mult, "WorldBank-multiple")

## This is the example from the animint paper, but using only single
## selection.
short.regions <- not.na %>%
  mutate(region=sub(" [(].*", "", region))
wb.paper.single <-
  list(ts=a_plot()+
       make_tallrect(short.regions, "year")+
       guides(color="none")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 showSelected="region",
                 clickSelects="country",
                 data=short.regions, size=4, alpha=3/5),
       scatter=a_plot()+
       a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=short.regions)+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year", "region"),
                 clickSelects="country",
                 data=short.regions)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(short.regions, 5, 85, "year"),
       time=list(variable="year", ms=3000),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (for Animint paper, single selection)")
animint2dir(wb.paper.single, "WorldBank-paper-single")

## This is the example from the animint paper, with multiple
## selection.
wb.paper <-
  list(ts=a_plot()+
       make_tallrect(short.regions, "year")+
       guides(color="none")+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 showSelected="region",
                 clickSelects="country",
                 data=short.regions, size=4, alpha=3/5),
       scatter=a_plot()+
       a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=short.regions)+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year", "region"),
                 clickSelects="country",
                 data=short.regions)+
       a_scale_size_animint(breaks=10^(5:9))+
       make_text(short.regions, 5, 85, "year"),
       time=list(variable="year", ms=3000),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple", region="multiple"),
       title="World Bank data (for Animint paper)")
info <- animint2dir(wb.paper, "WorldBank-paper")

BOTH <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}
TS <- function(df)BOTH(df, "Years", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS2 <- function(df)BOTH(df, "Fertility rate", "Years")
years <- unique(not.na[, "year", drop=FALSE])
min.years <- not.na %>%
  group_by(country) %>%
  filter(year==min(year)) %>%
  mutate(year=1958)
wb.facets <-
  list(ts=a_plot()+
       xlab("")+
       ylab("")+
       a_geom_tallrect(a_aes(xmin=year-1/2, xmax=year+1/2),
                     clickSelects="year",
                     data=TS(years), alpha=1/2)+
       a_theme_animint(width=1000, height=800)+
       a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                 clickSelects="country",
                 data=TS(not.na), size=4, alpha=3/5)+
       a_geom_point(a_aes(year, life.expectancy, color=region, size=population),
                  showSelected="country", clickSelects="country",
                  data=TS(not.na))+
       a_geom_text(a_aes(year, life.expectancy, colour=region, a_label=country),
                 showSelected="country",
                 clickSelects="country",
                  data=TS(min.years), hjust=1)+

       a_geom_widerect(a_aes(ymin=year-1/2, ymax=year+1/2),
                     clickSelects="year",
                     data=TS2(years), alpha=1/2)+
       a_geom_line(a_aes(fertility.rate, year, group=country, colour=region),
                 clickSelects="country",
                 data=TS2(not.na), size=4, alpha=3/5)+
       a_geom_point(a_aes(fertility.rate, year, color=region, size=population),
                  showSelected="country", clickSelects="country",
                  data=TS2(not.na))+

       a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population,
                      key=country), # key aesthetic for animated transitions!
                  clickSelects="country",
                  showSelected="year",
                  data=SCATTER(not.na))+
       a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                     key=country), #also use key here!
                 showSelected=c("country", "year", "region"),
                 clickSelects="country",
                 data=SCATTER(not.na))+
       a_scale_size_animint(breaks=10^(5:9))+
       a_facet_grid(side ~ top, scales="free")+
       a_geom_text(a_aes(5, 85, a_label=paste0("year = ", year)),
                 showSelected="year",
                 data=SCATTER(years)),
       
       time=list(variable="year",ms=3000),
       
       duration=list(year=1000),
       
       first=list(year=1975, country=c("United States", "Vietnam")),
       
       selector.types=list(country="multiple"),
       
       title="World Bank data (multiple selection, facets)")

animint2dir(wb.facets, "WorldBank-facets")

## Make a screencast to quickly show some animint features.

## system("mplayer -ao null screencast.ogv -vo jpeg:outdir=screencast")
## system("cp -r screencast screencast-small")
## jpg.vec <- Sys.glob("screencast-small/*.jpg")
## unlink(jpg.vec[seq_along(jpg.vec) %% 10 != 1])
## system("convert -resize 728x536 screencast-small/*.jpg -set delay 25 -layers Optimize screencast.gif")
