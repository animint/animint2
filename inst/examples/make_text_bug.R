#' NOAA SVRGIS data (Severe Report Database + Geographic Information System)
#' http://www.spc.noaa.gov/gis/svrgis/
#' Data - http://www.spc.noaa.gov/wcm/#data
#' Location Codes - http://www.spc.noaa.gov/wcm/loccodes.html
#' State FIPS Codes - http://www.spc.noaa.gov/wcm/fips_usa.gif
#' County FIPS Codes - http://www.spc.noaa.gov/wcm/stnindex_all.txt
#' State/County Area and Population - http://quickfacts.census.gov/qfd/download/DataSet.txt
#' 
#' Image Inspiration -  http://www.kulfoto.com/pic/0001/0033/b/h4n5832833.jpg
library(maps)
library(plyr)
library(animint2)
data(UStornadoes)
stateOrder <- data.frame(state = unique(UStornadoes$state)[order(unique(UStornadoes$TornadoesSqMile), decreasing=T)], rank = 1:49) # order states by tornadoes per square mile
UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
# useful for stat_bin, etc. 

USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]

UStornadoCounts <-
  ddply(UStornadoes, .(state, year), summarize, count=length(state))

## BUG: This one does not render the make_text in the second plot,
## why?
## 2017-08-24: TODO: Do we still have this bug? Find and fix
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
       bug=a_plot()+
       ggtitle("There should be state = XXX below")+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity")+
       a_geom_text(a_aes(year, count + 5, a_label=count),
                 showSelected=c("year", "state"),
                data=UStornadoCounts, size=20))
animint2dir(tornado.points.anim, "tornado-points-anim")

## Works. I moved the make_text after a_geom_bar.
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
       bug=a_plot()+
       ggtitle("There should be state = XXX below")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity")+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_text(a_aes(year, count + 5, a_label=count),
                 showSelected=c("year", "state"),
                data=UStornadoCounts, size=20))
animint2dir(tornado.points.anim, "tornado-points-anim")

## Works. I deleted the last a_geom_text.
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
       bug=a_plot()+
       ggtitle("There should be state = XXX below")+
       make_text(UStornadoes, 1980, 200, "state")+
       a_geom_bar(a_aes(year, count),
                clickSelects="year", showSelected="state",
                data=UStornadoCounts, a_stat="identity", a_position="identity"))
animint2dir(tornado.points.anim, "tornado-points-anim")

