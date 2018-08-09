#' NOAA SVRGIS data (Severe Report Database + Geographic Information System)
#' http://www.spc.noaa.gov/gis/svrgis/
#' Data - http://www.spc.noaa.gov/wcm/#data
#' Location Codes - http://www.spc.noaa.gov/wcm/loccodes.html
#' State FIPS Codes - http://www.spc.noaa.gov/wcm/fips_usa.gif
#' County FIPS Codes - http://www.spc.noaa.gov/wcm/stnindex_all.txt
#' State/County Area and Population - http://quickfacts.census.gov/qfd/download/DataSet.txt
#' 
#' Image Inspiration -  http://www.kulfoto.com/pic/0001/0033/b/h4n5832833.jpg
library(animint2)
data(UStornadoes)
stateOrder <- data.frame(state = unique(UStornadoes$state)[order(unique(UStornadoes$TornadoesSqMile), decreasing=T)], rank = 1:49) # order states by tornadoes per square mile
UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
# useful for stat_bin, etc. 

USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]

statemap <- a_plot() + a_geom_polygon(data=USpolygons, a_aes(x=long, y=lat, group=group), fill="black", colour="grey") +
  a_geom_segment(data=UStornadoes, a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth), colour="#55B1F7", alpha=.2) +
  a_geom_segment(data=UStornadoes, a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat, size=trackWidth, alpha=f), colour="#55B1F7") +
  a_scale_size_continuous("Width (yd)", range=c(.5, 2)) + 
  a_scale_alpha_continuous("Strength (F or EF scale)", range=c(.3, 1)) + 
  ggtitle("Tornado Paths, 1950-2006")

## ERROR: a_geom_bar + stat_bin + clickSelects does not make sense! We
## should stop with an error!
tornado.bar <-
  list(map=a_plot()+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour="#55B1F7", data=UStornadoes),
       ts=a_plot()+
       a_geom_bar(a_aes(year), clickSelects="year",data=UStornadoes))
animint2dir(tornado.bar, "tornado-bar")

## OK: a_stat_summary + clickSelects ensures unique x values.
tornado.bar <-
  list(map=a_plot()+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour="#55B1F7", data=UStornadoes),
       ts=a_plot()+
       a_stat_summary(a_aes(year, year),
                    clickSelects="year",
                    data=UStornadoes, fun.y=length, a_geom="bar"))
animint2dir(tornado.bar, "tornado-bar")

## Same plot, using make_bar abbreviation.
tornado.bar <-
  list(map=a_plot()+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour="#55B1F7", data=UStornadoes),
       ts=a_plot()+
       make_bar(UStornadoes, "year"))
animint2dir(tornado.bar, "tornado-bar")

UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))
## OK: select state to show that subset of bars!
tornado.ts.bar <-
  list(map=a_plot()+
       make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in %d")+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    clickSelects="state",
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour="#55B1F7", data=UStornadoes),
       ts=a_plot()+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity"))
animint2dir(tornado.ts.bar, "tornado-ts-bar")
## also show points.
seg.color <- "#55B1F7"
tornado.points <-
  list(map=a_plot()+
       make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in %d")+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    clickSelects="state",
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour=seg.color, data=UStornadoes)+
       ## a_geom_point(a_aes(startLong, startLat, fill=place, showSelected=year),
       ##              colour=seg.color,
       ##            data=data.frame(UStornadoes,place="start"))+
       a_scale_fill_manual(values=c(end=seg.color))+
       a_geom_point(a_aes(endLong, endLat, fill=place),
                  showSelected="year",
                  colour=seg.color,
                  data=data.frame(UStornadoes,place="end")),
       width=list(map=1500, ts=300),
       height=list(map=1000, ts=300),
       ts=a_plot()+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity"))
animint2dir(tornado.points, "tornado-points")

## It would be nice to be able to specify the width/height using
## animint.* theme options, but this currently gives an error... is
## there any work-around?
tornado.points <-
  list(map=a_plot()+
       make_text(UStornadoCounts, -100, 50, "year", "Tornadoes in %d")+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    clickSelects="state",
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour=seg.color, data=UStornadoes)+
       a_scale_fill_manual(values=c(end=seg.color))+
       a_theme_animint(width=750, height=500)+
       a_geom_point(a_aes(endLong, endLat, fill=place),
                  showSelected="year",
                  colour=seg.color,
                  data=data.frame(UStornadoes,place="end")),
       ts=a_plot()+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity"))
animint2dir(tornado.points, "tornado-points")

tornado.points.anim <-
  list(map=a_plot()+
       make_text(UStornadoes, -100, 50, "year",
                 "Tornado paths and endpoints in %d")+
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour=seg.color, data=UStornadoes)+
       a_geom_point(a_aes(endLong, endLat),
                  showSelected="year",
                  colour=seg.color,
                  data=UStornadoes)+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    clickSelects="state",
                    data=USpolygons, fill="grey", colour="black", alpha=3/4)+
       a_theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
             axis.ticks=a_element_blank(), axis.title=a_element_blank()),
       width=list(map=750, ts=300),
       height=list(map=500, ts=400),
       ##time=list(variable="year", ms=2000),
       ts=a_plot()+
       xlab("year")+
       ylab("Number of tornadoes")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity")+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_text(a_aes(year, count + 5, a_label=count),
                 showSelected=c("state", "year"),
                 data=UStornadoCounts, size=20))
animint2dir(tornado.points.anim, "tornado-points-anim")

## OK: interactive version with lines instead of bars!
tornado.ts.line <-
  list(map=a_plot()+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    clickSelects="state",
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour="#55B1F7", data=UStornadoes),
       ts=a_plot()+
       make_tallrect(UStornadoCounts, "year")+
       a_geom_line(a_aes(year, count, group=state),
                 clickSelects="state",
                 data=UStornadoCounts, alpha=3/5, size=4))
animint2dir(tornado.ts.line, "tornado-ts-line")


tornado.anim <-
  list(map=a_plot()+
       a_geom_polygon(a_aes(x=long, y=lat, group=group),
                    clickSelects="state",
                    data=USpolygons, fill="black", colour="grey") +
       a_geom_segment(a_aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                    showSelected="year",
                    colour="#55B1F7", data=UStornadoes),
       ts=a_plot()+
       make_tallrect(UStornadoCounts, "year")+
       a_geom_line(a_aes(year, count, group=state),
                 clickSelects="state",
                 data=UStornadoCounts, alpha=3/5, size=4),
       time=list(variable="year",ms=2000))
animint2dir(tornado.anim, "tornado-anim")
