library(animint2)
data(WorldBank)
WorldBank$Region <- sub(" [(].*", "", WorldBank$region)
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
subset(not.na, country == "Kuwait" & 1991 <= year & year <= 1995)
not.na[not.na$country=="Kuwait", "population"] <- 1700000
BOTH <- function(df, top, side)data.frame(
  df,
  top=factor(top, c("Fertility rate", "Year")),
  side=factor(side, c("Year", "Life expectancy")))
TS <- function(df)BOTH(df, "Year", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS_FERT <- function(df)BOTH(df, "Fertility rate", "Year")
MAP <- function(df)BOTH(df, "Year", "Year")
years <- unique(not.na[, "year", drop=FALSE])
by.country <- split(not.na, not.na$country)
first.year <- min(not.na$year)
last.year <- max(not.na$year)
min.years <- do.call(rbind, lapply(by.country, subset, year == min(year)))
min.years$year <- 1959.5
year.breaks <- seq(1960,2010,by=10)
world_map <- animint2::map_data("world")
map_names <- c(x="long", y="lat")
map_df <- if(requireNamespace("rmapshaper")){
  ## https://cloud.r-project.org/web/packages/sf/vignettes/sf1.html section "sfg: simple feature geometry" explains st_polygon represents a single polygon.
  library(data.table)
  (map_dt <- data.table(world_map))
  (poly_dt <- map_dt[, .(original=sf::st_sfc(
    sf::st_polygon(list(cbind(long, lat)))
  )), by=.(region,group)][
    ## https://datascience.blog.wzb.eu/2021/03/15/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper explains that this simplification respects boundaries between adjacent polygons.
  , simple := rmapshaper::ms_simplify(original, keep_shapes=TRUE, keep=0.01)
  ][])
  sapply(poly_dt[,.(original,simple)], sapply, function(x)nrow(x[[1]]))
  compare_dt <- data.table(type=c("original","simple"))[, {
    poly_dt[
    , as.data.table(.SD[[type]][[1]][[1]])
    , by=.(region, group)]
  }, by=type][, rows := .N, by=type]
  gg <- ggplot()+
    geom_polygon(aes(
      long, lat,
      group=group),
      color="black",
      fill="grey50",
      data=compare_dt)+
    facet_grid(type + rows ~ ., labeller=label_both)
  gg
<<<<<<< Updated upstream
  gg+coord_cartesian(xlim=c(-20, 0),ylim=c(10, 40))+
    geom_text(aes(
      long, lat, label=region),
      data=compare_dt)
=======
  ## zoom to Western Sahara.
  group_means <- compare_dt[
  , lapply(.SD, mean)
  , by=.(region,group)
  , .SDcols=c("lat","long")]
  gg+coord_cartesian(xlim=c(-20, 0),ylim=c(10, 40))+
    geom_text(aes(
      long, lat, label=region),
      data=group_means)
>>>>>>> Stashed changes
  as.data.frame(compare_dt[type=="simple"])
}else{
  world_map
}
country2Region <- with(unique(not.na[, c("Region","country")]), structure(Region, names=country))
map2wb <- c(
  Antigua="Antigua and Barbuda",
  Brunei="Brunei Darussalam",
  Bahamas="Bahamas, The", 
  "Democratic Republic of the Congo"="Congo, Dem. Rep.",
  "Republic of Congo"="Congo, Rep.",
  "Ivory Coast"="Cote d'Ivoire",
  Egypt="Egypt, Arab Rep.", 
  Micronesia="Micronesia, Fed. Sts.",
  UK="United Kingdom",
  Gambia="Gambia, The",
  ##"Hong Kong SAR, China", 
  Iran="Iran, Islamic Rep.",
  ##"Channel Islands",
  Kyrgyzstan="Kyrgyz Republic",
  "Saint Kitts"="St. Kitts and Nevis", 
  "North Korea"="Korea, Dem. Rep.",
  "South Korea"="Korea, Rep.",
  Laos="Lao PDR",
  "Saint Lucia"="St. Lucia",
  "North Macedonia"="Macedonia, FYR", 
  ##"Macao SAR, China",
  Palestine="West Bank and Gaza",
  Russia="Russian Federation", 
  Slovakia="Slovak Republic",
  "Saint Martin"="Sint Maarten (Dutch part)",
  Syria="Syrian Arab Republic", 
  Trinidad="Trinidad and Tobago",
  Tobago="Trinidad and Tobago",
  USA="United States",
  "Saint Vincent"="St. Vincent and the Grenadines", 
  Venezuela="Venezuela, RB",
  "Virgin Islands"="Virgin Islands (U.S.)",
  Yemen="Yemen, Rep.")
map_disp <- with(map_df, data.frame(
  group, country=ifelse(region %in% names(map2wb), map2wb[region], region)))
map_disp$Region <- country2Region[map_disp$country]
for(new.var in names(map_names)){
  old.var <- map_names[[new.var]]
  old.val <- map_df[[old.var]]
  m <- min(old.val)
  old.01 <- (old.val-m)/(max(old.val)-m)
  map_disp[[new.var]] <- old.01*(last.year-first.year)+first.year
}
unot <- function(DFu, DFref)unique(subset(DFu, !country %in% DFref$country)$country)
unot(map_disp, not.na)
dput(unot(not.na, map_disp))
wb.facets <- animint(
  ts=ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1100, height=600)+
    facet_grid(side ~ top, scales="free")+
    ## TS_FERT
    make_widerect(
      not.na, "year", data.fun=TS_FERT,
      title="Grey rectangle year selector")+
    geom_label_aligned(aes(
      fertility.rate, year,
      key=country,
      colour=Region, label=country),
      showSelected="country",
      clickSelects="country",
      help="Names of selected countries.",
      alignment="horizontal",
      data=TS_FERT(min.years),
      vjust=1)+
    geom_path(aes(
      fertility.rate, year,
      key=country, group=country,
      colour=Region),
      clickSelects="country",
      help="Time series of fertility rate, one line per country.",
      data=TS_FERT(not.na),
      size=4,
      alpha=1,
      alpha_off=0.1)+
    ## MAP
    geom_polygon(aes(
      x, y,
      key=group,
      group=group,
      fill=Region),
      title="World map",
      clickSelects="country",
      color="black",
      color_off="transparent",
      alpha=1,
      alpha_off=0.3,
      data=MAP(map_disp))+
    ## SCATTER
    geom_point(aes(
      fertility.rate, life.expectancy, colour=Region, size=population,
      key=country), # key aesthetic for smooth transitions!
      clickSelects="country",
      showSelected="year",
      help="Scatter plot for the selected year, one point per country.",
      alpha=1,
      alpha_off=0.3,
      chunk_vars=character(),
      data=SCATTER(not.na))+
    geom_text(aes(
      fertility.rate, life.expectancy, label=country,
      key=country), #also use key here!
      showSelected=c("country", "year", "Region"),
      clickSelects="country",
      alpha=0.7,
      help="Names of selected countries.",
      chunk_vars=character(),
      data=SCATTER(not.na))+
    geom_text(aes(
      5, 85, label=paste0("year = ", year),
      key=1),
      showSelected="year",
      title="Selected year",
      data=SCATTER(years))+
    ## TS
    make_tallrect(
      not.na, "year", data.fun=TS,
      title="Grey rectangle year selector")+
    geom_line(aes(
      year, life.expectancy,
      key=country, group=country,
      colour=Region),
      clickSelects="country",
      help="Time series of life expectancy, one line per country.",
      data=TS(not.na),
      size=4,
      alpha=1,
      alpha_off=0.1)+
    geom_label_aligned(aes(
      year, life.expectancy,
      key=country,
      colour=Region, label=country),
      showSelected="country",
      clickSelects="country",
      help="Names of selected countries.",
      data=TS(min.years),
      hjust=1)+
    ## SCALES
    scale_size_animint(breaks=10^(9:5))+
    scale_x_continuous(
      "",
      breaks=c(year.breaks, 1:9))+
    scale_y_continuous(
      "",
      breaks=c(year.breaks, seq(25,85,by=10))
    ),
  time=list(variable="year", ms=2000),
  duration=list(year=1000, country=1000, Region=1000),
  first=list(year=1975, country=c("United States", "Canada", "France", "Japan")),
  selector.types=list(country="multiple"),
  source="https://github.com/animint/animint2/blob/master/inst/examples/WorldBank-facets-map.R",
  out.dir="WorldBank-facets-map",
  video="https://vimeo.com/1050117030",
  title="World Bank data (multiple selection, facets, map)")
if(Sys.which("firefox")!="")options(browser="firefox")
wb.facets
if(FALSE){
  animint2pages(wb.facets, "2025-01-WorldBank-facets-map")
}
