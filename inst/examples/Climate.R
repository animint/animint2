#' 2006 Data Expo: 
#' Data source: 
#' NASA Goddard Institute for Space Studies (GISS)
#' subset of the monthly climatology of the International 
#' Satellite Cloud Climatology Project (ISCCP), which was
#' developed “to collect weather satellite radiance
#' measurements and to analyze them to infer the
#' global distribution of clouds, their properties, and
#' their diurnal, seasonal and interannual variations.”
#' 
#' Data contains: Monthly observations of atmospheric variables 1995-2000, 
#' between 113.75ºW-56.25ºW, 21.25ºS-36.25ºN with 2.5º grid
#' spacing.
#' 
#' Variables: pressure, temperature, ozone and low, medium, 
#' high cloud cover.
#' The pressure, ozone and near-surface temperature
#' observations are computed from the TIROS Optical Vertical 
#' Sounder (TOVS) satellite climatology observations
#'
#' Temperatures are given in degrees celsius (original data had Kelvin).

library(animint2)
library(maps)
library(lubridate)
library(plyr)

data(climate)
# climate$time2 <- climate$year + floor(climate$month/3)/4 
climate$time2 <- decimal_date(ymd(as.character(climate$date)))
countries <- map_data("world")
countries <- subset(countries, (lat < 38)&(lat>-24))
countries <- subset(countries, ((-long)>54)&((-long)<118))


temp.seq <- a_plot() + 
  make_tallrect(data=climate, "time2") + 
  a_geom_line(data=climate, a_aes(x=time2, y=temperature), showSelected="id")

clouds.high <- a_plot() + 
  a_geom_tile(data=climate, a_aes(x=long, y=lat, fill=cloudhigh),
            clickSelects="id", showSelected="time2", colour="grey")+ 
  a_scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  a_geom_path(data=countries, a_aes(x=long, y=lat, group=group)) + 
  ggtitle("High Altitute Cloud Cover")+ 
  a_theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank())

clouds.mid <- a_plot() + 
  a_geom_tile(data=climate, a_aes(x=long, y=lat, fill=cloudmid),
            clickSelects="id", showSelected="time2", colour="grey")+ 
  a_scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  a_geom_path(data=countries, a_aes(x=long, y=lat, group=group)) + 
  ggtitle("Mid Altitute Cloud Cover")+ 
  a_theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank())

clouds.low <- a_plot() + 
  a_geom_tile(data=climate, a_aes(x=long, y=lat, fill=cloudlow),
            clickSelects="id", showSelected="time2", colour="grey")+ 
  a_scale_fill_gradient("Coverage", low="skyblue", high="white", limits=c(0, 75)) + 
  a_geom_path(data=countries, a_aes(x=long, y=lat, group=group)) + 
  ggtitle("Low Altitute Cloud Cover")+ 
  a_theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank())

ozone.map <- a_plot() + 
  a_geom_tile(data=climate, a_aes(x=long, y=lat, fill=ozone),
            clickSelects="id", showSelected="time2", colour="grey")+ 
  a_scale_fill_gradient("Concentration", low="white", high="brown") + 
  a_geom_path(data=countries, a_aes(x=long, y=lat, group=group)) + 
  ggtitle("Ozone Concentration")+ 
  a_theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank())

# Create variable showing temp-avg.monthly.temp at that location
climate <- ddply(climate, .(id, month), transform, tempdev = temperature - mean(temperature))

temperature.map <- a_plot() + 
  a_geom_tile(data=climate, a_aes(x=long, y=lat, fill=tempdev),
            clickSelects="id", showSelected="time2", colour="grey")+ 
  a_scale_fill_gradient2("Temperature", low="blue", mid="white", high="red", limits=c(-20, 20), midpoint=0) + 
  a_geom_path(data=countries, a_aes(x=long, y=lat, group=group)) + 
  ggtitle("Temperature Deviation from Monthly Norm")+ 
  a_theme(axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank())

animint2dir(list(temperature = temp.seq, 
                cloudslow = clouds.low, 
                cloudsmid = clouds.mid, 
                cloudshigh = clouds.high, 
                ozone = ozone.map,
                tempmap = temperature.map,
                time = list(variable="time2", ms=3000),
                width = list(450),
                height = list(450)
                ))
