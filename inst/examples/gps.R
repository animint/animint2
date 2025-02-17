if(!file.exists("gspdb-code")){
  system("hg clone http://hg.code.sf.net/p/gpsdb/code gpsdb-code")
}
gpx.glob <- "gpsdb-code/gpx/*"
library(data.table)
data.table(gpx=Sys.glob(gpx.glob))
f <- function(name)nc::field(
  name, '="', "[0-9.]+",
  as.numeric)
read_lat_lon <- function(path)nc::capture_all_str(
  path,
  f("lat"),
  '" ',
  f("lon")
)[, .( # these files have lat/lon inverted.
  longitude=lat,
  latitude=lon
)]
(lat.lon.dt <- nc::capture_first_glob(
  gpx.glob,
  "/", timestamp="[^/]+", "[.]gpx$",
  READ=read_lat_lon))
(ride.dt <- lat.lon.dt[, .(
  N_data=.N,
  ## https://r-spatial.github.io/sf/reference/geos_measures.html TODO kilometers
  pct = as.POSIXct(strptime(timestamp, "%Y-%m-%d:%H:%M:%S"))
), by=timestamp])

tag.lines <- readLines("~/gpsdb-code/kml/tags.txt")
tag.dt <- unique(nc::capture_first_vec(
  tag.lines,
  date_pattern=".*?",
  " ",
  riders=".*"))
date2stamp <- tag.dt[, .(
  timestamp=grep(date_pattern, ride.dt$timestamp, fixed=TRUE, value=TRUE)
), by=date_pattern]
stamp2riders <- tag.dt[date2stamp,on="date_pattern"]
riders2rider <- tag.dt[, .(
  rider=strsplit(riders, " ")[[1]]
), by=riders]
ride.with.riders <- stamp2riders[
  ride.dt,on="timestamp"
][
  is.na(riders), riders := ""
][]
ride.show <- ride.with.riders[riders!="florent" & N_data > 10]
path.show <- lat.lon.dt[timestamp %in% ride.show$timestamp]

library(animint2)
start.end.dt <- path.show[
, .SD[c(1,.N), .(
  latitude,longitude,where=c("start","end"),
  direction=diff(longitude)
)]
, by=timestamp]
ile_de_france.json <- "departements-ile-de-france.geojson"
if(!file.exists(ile_de_france.json)){
  u <- paste0(
    "https://france-geojson.gregoiredavid.fr/repo/regions/ile-de-france/",
    ile_de_france.json)
  download.file(u, ile_de_france.json)
}
##ile_de_france.list <- RJSONIO::fromJSON(ile_de_france.json)
ile_de_france.sf <- geojsonsf::geojson_sf(ile_de_france.json)
names(ile_de_france.sf)
class(ile_de_france.sf)
ile_de_france.dt <- data.table(ile_de_france.sf)[, {
  setnames(data.table(geometry[[1]][[1]]), c("longitude","latitude"))
}, by=.(code,nom)]

if(!file.exists("gps-villes-de-france.csv")){
  download.file("https://www.data.gouv.fr/fr/datasets/r/51606633-fb13-4820-b795-9a2a575a72f1", "gps-villes-de-france.csv")
}
villes.dt <- fread("gps-villes-de-france.csv")
map.lim <- dcast(
  path.show,
  . ~ .,
  fun.aggregate = list(min, max),
  value.var=c("latitude","longitude"))
villes.in.lim <- villes.dt[
  map.lim$latitude_min < latitude & latitude < map.lim$latitude_max &
    map.lim$longitude_min < longitude & longitude < map.lim$longitude_max
][
, long_name := sprintf(
  "%s (%s), %s (%s), %s",
  label, insee_code,
  department_name, department_number, region_geojson_name)
][]

dt2point <- function(DT){
  DT[, sf::st_sfc(apply(
    cbind(longitude, latitude),
    1,
    sf::st_point,
    simplify=FALSE
  )) |> sf::st_set_crs(4326)]
}
villes.sf <- sf::st_sf(villes.in.lim[, .(
  label,
  geometry=dt2point(.SD)
)])
path.sf <- sf::st_sf(path.show[, .(
  geometry=sf::st_sfc(sf::st_linestring(cbind(longitude, latitude))) |>
    sf::st_set_crs(4326)
), by=timestamp])
data.table(path.sf)
start.end.sf <- sf::st_sf(start.end.dt[, .(
  timestamp, where,
  geometry=dt2point(.SD)
)])
dist.dt <- data.table(start.end.sf)[, .(
  dist_units=sf::st_distance(geometry[1], geometry[2])
), by=timestamp
][
, kilometers := as.numeric(dist_units)/1000
][
  ride.show, on="timestamp"
]
dist.path <- path.show[
, geometry := dt2point(.SD)
][, .(
  dist_units_total=sum(sf::st_distance(
    geometry[-1],geometry[-.N], by_element = TRUE))
), by=timestamp][
, kilometers := as.numeric(dist_units_total)/1000
][
  ride.show, on="timestamp"
]
both.dist <- data.table(dist.path, dist.dt)
ggplot()+
  theme_bw()+
  geom_abline(slope=1,intercept=0,color="grey")+
  geom_point(aes(
    as.numeric(dist_units), as.numeric(dist_units_total)),
    data=both.dist)
nearest.index.vec <- sf::st_nearest_feature(start.end.sf, villes.sf)
villes.start.end <- data.table(
  start.end.dt[,.(timestamp,direction,where)],
  villes.in.lim[nearest.index.vec])
## villes near path.
with.mat <- sf::st_is_within_distance(
  path.sf, villes.sf,
  dist=1000)#meters
villes.show.list <- list()
for(path.i in seq_along(with.mat)){
  ville.i <- with.mat[[path.i]]
  if(length(ville.i)){
    villes.show.list[[path.i]] <- data.table(
      villes.in.lim[ville.i],
      as.data.table(path.sf[path.i,])[,.(timestamp)])
  }
}
(villes.near.path <- rbindlist(villes.show.list))

km.text.x <- 1
km.text.y <- 48.4
text.hjust <- 0
villes.start.end[, let(
  what="nearby cities",
  text.x=km.text.x,
  text.y=ifelse(where=="start", 48.3, 48.2))]
city.text.size <- 15
where.colors <- c(start="white",end="black")
viz <- animint(
  title="Map and time series of Toby Hocking's Bike Rides in 2009",
  source="https://github.com/animint/animint2/blob/master/inst/examples/gps.R",
  video="https://vimeo.com/1048533960",
  map=ggplot()+
    ggtitle("Map of rides, click to select ride")+
    theme_bw()+
    theme_animint(width=1000)+
    coord_cartesian(expand=FALSE)+
    scale_fill_manual(
      values=where.colors,
      breaks=names(where.colors))+
    geom_polygon(aes(
      longitude, latitude, group=nom,
      tooltip=sprintf("%s (%s)", nom, code),
      color=what),
      alpha=0.5,
      data=data.table(
        what="Dept. en IDF",
        ile_de_france.dt),
      fill="yellow")+
    geom_path(aes(
      longitude, latitude, group=timestamp),
      clickSelects="timestamp",
      color_off="grey",
      color="grey40",
      help="All rides displayed in grey.",
      size=4,
      data=data.table(path.show, what="ride"))+
    geom_path(aes(
      longitude, latitude,
      key=1),
      showSelected="timestamp",
      color="black",
      help="Selected ride shown in black.",
      chunk_vars=character(),
      size=4,
      data=data.table(path.show, what="ride"))+
    geom_point(aes(
      longitude, latitude,
      key=where,
      fill=where, color=what),
      help="Start and end of selected ride.",
      showSelected="timestamp",
      size=4,
      data=data.table(start.end.dt, what="ride"))+
    scale_color_manual(values=c(
      ride="black",
      "Dept. en IDF"="orange",
      "nearby cities"="red"))+
    geom_point(aes(
      longitude, latitude,
      key=where,
      fill=where, color=what),
      help="Cities nearest to start and end of selected ride.",
      showSelected=c("timestamp","where"),
      data=villes.start.end)+
    geom_point(aes(
      longitude, latitude, color=what,
      key=long_name,
      tooltip=long_name),
      help="Cities near the selected ride (hover cursor to show details).",
      showSelected="timestamp",
      data=data.table(villes.near.path, what="nearby cities"))+
    geom_text(aes(
      text.x, text.y, color=what,
      key=where,
      label=sprintf(
        "%s: %s", where, long_name)),
      hjust=text.hjust,
      help="Details of cities nearest to start and end of selected ride.",
      showSelected=c("timestamp","where"),
      data=villes.start.end,
      size=city.text.size)+
    geom_text(aes(
      km.text.x, km.text.y,
      color=what,
      key=1,
      label=sprintf("%.1f kilometers", kilometers)),
      hjust=text.hjust,
      help="Number of kilometers along path.",
      size=city.text.size,
      showSelected="timestamp",
      data=data.table(what="ride", dist.path))+
    geom_text(aes(
      longitude, latitude, label=label,
      key=where,
      color=what,
      hjust=ifelse(
        direction<0,
        ifelse(where=="start", 0, 1),
        ifelse(where=="start", 1, 0))),
      help="Cities nearest to start and end of selected ride.",
      showSelected=c("timestamp","where"),
      size=city.text.size,
      data=villes.start.end),
  ts=ggplot()+
    ggtitle("Time series of rides, click to select ride")+
    theme_bw()+
    theme_animint(width=1000, height=200)+
    scale_y_continuous("Kilometers")+
    geom_point(aes(
      pct, kilometers),
      clickSelects="timestamp",
      help="One point for each ride.",
      size=5,
      alpha=0.7,
      data=dist.path)+
    geom_point(aes(
      pct, kilometers,
      key=1),
      help="Selected ride.",
      showSelected="timestamp",
      size=5,
      data=dist.path)+
    scale_x_datetime("Date/time of ride", date_breaks="1 month"),
  out.dir="gps",
  duration=list(timestamp=1000),
  time=list(
    variable="timestamp",
    ms=1500
  )
)
viz
if(FALSE){
  animint2pages(viz, "2025-01-19-bike-rides-around-paris-2009")
}
