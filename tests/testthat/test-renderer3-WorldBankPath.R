acontext("WorldBankPath")

data(WorldBank, package = "animint2")
library(animint2)
data(WorldBank)

unique_years <- unique(WorldBank$year)
WorldBankLast5 <- data.frame()

for (y in unique_years ) {
  last_five <- subset(WorldBank, y-5 <= year & year < y)
  if (nrow(last_five) > 0) {
    last_five$year <- y
    WorldBankLast5  <- rbind(WorldBankLast5, last_five)
  } 
}

scatter <- ggplot()+
  geom_point(aes(
    x=life.expectancy, 
    y=fertility.rate, 
    color=region,
    key=country),
    showSelected="year",
    clickSelects="country",
    data=WorldBank)+
  geom_path(aes(x=life.expectancy,
                y=fertility.rate, 
                color=region,
                key=country, 
                group=country),
            showSelected="year",  
            data=WorldBankLast5)  
viz.scatter <- animint(scatter,   
                       duration=list(year=2000),
                       title="Worldbank data last 5 years", 
                       source="https://github.com/siddhesh195/worldbank_geom_path/main/TestEasy.R")

viz.worldbank <- viz.scatter
viz.worldbank$first <- list(
  year=2000)
viz.worldbank
info <- animint2HTML(viz.worldbank)

# Test that all circle and path are rendered for year = 2000
# The number of circle and path are different because
# of missing data points in WorldBank data set

test_that("192 <circle> rendered for all countries", {
  countries=getNodeSet(info$html, '//g[@class="geom1_point_scatter"]//circle')
  expect_equal(length(countries), 192)
})

test_that("198 <path> rendered for all countries", {
  countries=getNodeSet(info$html, '//g[@class="geom2_path_scatter"]//path')
  expect_equal(length(countries), 198)
})

