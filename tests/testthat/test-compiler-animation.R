acontext("animation")

if(require(maps) && require(plyr)){
  data(UStornadoes, package = "animint2")
  stateOrder <- data.frame(state = unique(UStornadoes$state)[order(unique(UStornadoes$TornadoesSqMile), decreasing=T)], rank = 1:49) # order states by tornadoes per square mile
  UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
  UStornadoes$weight <- 1/UStornadoes$LandArea
  # useful for stat_bin, etc.
  USpolygons <- map_data("state")
  USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]
  UStornadoCounts <-
    ddply(UStornadoes, .(state, year), summarize, count=length(state))
  tornado.anim <- list(
    map=ggplot()+
      geom_polygon(aes(
        x=long, y=lat, group=group),
        data=USpolygons,
        clickSelects="state",
        fill="black", colour="grey") +
      geom_segment(aes(
        x=startLong, y=startLat, xend=endLong, yend=endLat),
        showSelected="year",
        colour="#55B1F7", data=UStornadoes),
    ts=ggplot()+
      make_tallrect(UStornadoCounts, "year")+
      geom_line(aes(
        year, count, group=state),
        clickSelects="state",
        data=UStornadoCounts, alpha=3/5, size=4),
    time=list(variable="year",ms=2000))
  test_that("tornado animation frames correct", {
    info <- animint2dir(tornado.anim, open.browser=FALSE)
    expect_identical(info$time$sequence, as.character(1950:2012))
  })
}

## WorldBank/gapminder example.
data(WorldBank, package = "animint2")
motion <-
  list(scatter=ggplot()+
         geom_point(aes(life.expectancy, fertility.rate,
                        colour=region, size=population, key=year),
                        clickSelects="country",
                        showSelected="year",
                  data=WorldBank)+
       make_text(WorldBank, 55, 9, "year")+
       scale_size_continuous(range=c(1.5,20)),
       ts=ggplot()+
       make_tallrect(WorldBank, "year")+
       geom_line(aes(year, life.expectancy, group=country),
                 clickSelects="country",
                 data=WorldBank, size=4, alpha=3/5),
       time=list(variable="year",ms=3000),
       duration=list(year=1000))

test_that("WorldBank animation frames correct", {
  info <- animint2dir(motion, open.browser=FALSE)
  expect_identical(info$time$sequence, as.character(1960:2012))
})

## Evolution.
data(generation.loci, package = "animint2")
## Example: 2 plots, 2 selectors.
generations <- data.frame(generation=unique(generation.loci$generation))
loci <- data.frame(locus=unique(generation.loci$locus))
first <- subset(generation.loci,generation==1)
ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
  stopifnot(all(frequency==frequency[1]))
  data.frame(locus=locus[1],ancestral=frequency[1])
}))
gl.list <- split(generation.loci,
                 with(generation.loci,list(generation,locus)))
generation.pop <- do.call(rbind,lapply(gl.list,with,{
  data.frame(generation=generation[1], locus=locus[1],
             estimated=mean(frequency))
}))
generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]
evolution <- 
  list(ts=ggplot()+
         geom_vline(aes(xintercept=generation),
                    clickSelects="generation",
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population),
                   showSelected="locus",
                   data=generation.loci),
       predictions=ggplot()+
         geom_point(aes(ancestral, estimated, key=locus),
                    showSelected="generation",
                    clickSelects="locus",               
                    data=generation.pop, size=4, alpha=3/4),
       loci=ggplot()+
         geom_vline(aes(xintercept=locus),
                    data=loci,
                    clickSelects="locus",
                    alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency, key=locus),
                    showSelected="generation",
                    data=generation.loci),
       duration=list(generation=1000),
       time=list(variable="generation",ms=2000))

test_that("tornado animation frames correct", {
  info <- animint2dir(evolution, open.browser=FALSE)
  expect_identical(info$time$sequence, as.character(1:100))
})
