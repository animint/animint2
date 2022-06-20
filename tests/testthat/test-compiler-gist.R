acontext("gists")

test_that("animint2gist() returns an object of class 'gist'", {
  g <- animint2gist(list(p = qplot(1:10)), browse = FALSE)
  expect_is(g, "gist")
  gistr::delete(g)
})

data(WorldBank, package = "animint2")
not.na <- subset(WorldBank, !(is.na(life.expectancy) | is.na(fertility.rate)))
subset(not.na, is.na(not.na$population))
subset(not.na, country == "Kuwait" & 1991 <= year & year <= 1995)
not.na[not.na$country=="Kuwait", "population"] <- 1700000
BOTH <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}
TS <- function(df)BOTH(df, "Years", "Life expectancy")
SCATTER <- function(df)BOTH(df, "Fertility rate", "Life expectancy")
TS2 <- function(df)BOTH(df, "Fertility rate", "Years")
years <- unique(not.na[, "year", drop=FALSE])
by.country <- split(not.na, not.na$country)
min.years <- do.call(rbind, lapply(by.country, subset, year == min(year)))
min.years$year <- 1958

viz.chunk.none <- 
  list(ts=ggplot()+
         theme_bw()+
         theme(panel.margin=grid::unit(0, "lines"))+
         xlab("")+
         ylab("")+
         geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2),
                       clickSelects="year",
                       data=TS(years), alpha=1/2)+
         theme_animint(width=1000, height=800)+
         geom_line(aes(year, life.expectancy, group=country, colour=region),
                   clickSelects="country",                   
                   data=TS(not.na), size=4, alpha=3/5)+
         geom_point(aes(year, life.expectancy, color=region, size=population),
                    data=TS(not.na),
                    showSelected="country",
                    clickSelects="country")+
         geom_text(aes(year, life.expectancy, colour=region, label=country),
                   data=TS(min.years),
                   showSelected="country",
                   clickSelects="country",
                   hjust=1)+
         geom_widerect(aes(ymin=year-1/2, ymax=year+1/2),
                       data=TS2(years), alpha=1/2,
                           clickSelects="year")+
         geom_path(aes(fertility.rate, year, group=country, colour=region),
                   data=TS2(not.na), size=4, alpha=3/5,
                       clickSelects="country")+
         geom_point(aes(fertility.rate, year, color=region, size=population),
                    data=TS2(not.na),
                        showSelected="country", clickSelects="country")+
         geom_point(aes(fertility.rate, life.expectancy,
                        key=country,
                        colour=region, size=population), 
                    chunk_vars=c(),
                    clickSelects="country",
                    showSelected="year",
                    data=SCATTER(not.na),
                    validate_params = FALSE)+
         geom_text(aes(fertility.rate, life.expectancy,
                       key=country,
                       label=country), 
                   chunk_vars=c(),
                   showSelected=c("country", "year", "region"),
                   clickSelects="country",
                   data=SCATTER(not.na),
                   validate_params = FALSE)+
         scale_size_animint(breaks=10^(5:9))+
         facet_grid(side ~ top, scales="free")+
         geom_text(aes(5, 85, label=paste0("year = ", year), key=year),
                       showSelected="year",
                   data=SCATTER(years)),
       time=list(variable="year",ms=3000),
       duration=list(year=1000),
       first=list(year=1975, country=c("United States", "Vietnam")),
       selector.types=list(country="multiple"),
       title="World Bank data (multiple selection, facets)")

test_that("too big files error", {
  expect_error({
    animint2gist(viz.chunk.none)
  }, "files bigger than 1MB")
})

set.seed(1)
nrows <- 300
too.many <- data.frame(row=1:nrows, x=rnorm(nrows), y=rnorm(nrows))
too.tall.list <- list()
for(col.name in c("x", "y")){
  too.tall.list[[col.name]] <-
    data.frame(col.name,
               row=1:nrows,
               value=too.many[[col.name]])
}
too.tall <- do.call(rbind, too.tall.list)

viz.too.many <-
  list(points=ggplot()+
         geom_point(aes(x, y),
                    data=too.many, clickSelects="row"),
       bars=ggplot()+
         geom_bar(aes(col.name, value),
                  chunk_vars=c("row"), showSelected="row",
                  stat="identity",
                  position="identity",
                  data=too.tall,
                  validate_params = FALSE))
    
test_that("too many files error", {
  expect_error({
    animint2gist(viz.too.many)
  }, "the Gist API will not serve more than 300 files")
})
