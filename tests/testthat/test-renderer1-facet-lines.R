acontext("facet lines")

data(WorldBank, package = "animint2")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
TS <- function(df)data.frame(df, a_facet="Years")
SCATTER <- function(df)data.frame(df, a_facet="Fertility rate")
years <- unique(not.na[, "year", drop=FALSE])
countries <- unique(not.na[, "country", drop=FALSE])
wb.facets <-
  list(ts=a_plot()+
         xlab("")+
         a_geom_tallrect(a_aes(xmin=year-1/2, xmax=year+1/2),
                       clickSelects="year",
                       data=TS(years), alpha=1/2)+
         a_theme_animint(width=1000)+
         a_geom_line(a_aes(year, life.expectancy, group=country, colour=region),
                   clickSelects="country", showSelected="region",
                   data=TS(not.na), size=4, alpha=3/5)+
         a_geom_point(a_aes(year, life.expectancy, color=region, size=population),
                    showSelected=c("country", "region"), clickSelects="country",
                    data=TS(not.na))+
         a_geom_point(a_aes(fertility.rate, life.expectancy, colour=region, size=population,
                        key=country), # key aesthetic for animated transitions!
                    showSelected=c("year", "region"), clickSelects=c("country"),
                    data=SCATTER(not.na))+
         a_geom_text(a_aes(fertility.rate, life.expectancy, a_label=country,
                       key=country), #also use key here!
                   showSelected=c("country", "year"),
                   clickSelects="country",
                   data=SCATTER(not.na))+
         a_scale_size_animint(breaks=10^(5:9))+
         a_facet_grid(.~a_facet, scales="free")+
         a_geom_text(a_aes(5, 85, a_label=paste0("year = ", year)),
                   showSelected="year",
                   data=SCATTER(years)),
       time=list(variable="year",ms=3000),
       bar=a_plot()+
         a_theme_animint(height=2400)+
         a_geom_bar(a_aes(country, life.expectancy, fill=region,
                      key=country),
                  showSelected="year", clickSelects="country",
                  data=not.na, a_stat="identity", a_position="identity")+
         a_coord_flip(),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection, facets)")

remDr$setTimeout(milliseconds=60 * 1000 * 2)
info <- animint2HTML(wb.facets)

test_that("if group is in nest_order, it is last", {
  for(g in info$geoms){
    nest <- as.character(g$nest_order)
    if("group" %in% nest){
      expect_match(nest[length(nest)], "group")
    }
  }
})

test_that("a <path> is rendered for every country", {
  node.set <- getNodeSet(info$html, '//g[@class="a_geom2_line_ts"]//path')
  expect_equal(length(node.set), nrow(countries))
})
