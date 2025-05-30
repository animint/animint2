acontext("save separate chunks")
library(plyr)

data(FluView, package = "animint2")
# use one season to test
state_flu <- subset(FluView$state_flu, SEASON == "2008-09")
flu.by.weekend <- split(state_flu, state_flu$WEEKEND)
map.by.weekend <- list()
for(WEEKEND in names(flu.by.weekend)){
  one.weekend <- flu.by.weekend[[WEEKEND]]
  rownames(one.weekend) <- one.weekend$state
  map.flu <- subset(FluView$USpolygons, select=-order)
  map.flu$level <- one.weekend[map.flu$region, "level"]
  map.by.weekend[[WEEKEND]] <- data.frame(WEEKEND, map.flu)
}
map_flu <- do.call(rbind, map.by.weekend)

# visualize CDC FluView data
# activity level heatmap
level.heatmap <- ggplot() + 
  geom_tile(aes(x = WEEKEND, y = STATENAME, fill = level),
            data = state_flu) + 
  geom_tallrect(aes(xmin = WEEKEND - 3, xmax = WEEKEND + 3), 
                data = state_flu, alpha = .5,
                    clickSelects = "WEEKEND") + 
  scale_x_date(expand = c(0, 0)) + 
  scale_fill_gradient2(low = "white", high = "red", breaks = 0:10) + 
  theme_animint(width = 1200, height = 700) + 
  ggtitle("CDC ILI Activity Level in Lower 48 States")

# state map
theme_opts <- list(theme(panel.grid.minor = element_blank(), 
                         panel.grid.major = element_blank(), 
                         panel.background = element_blank(), 
                         panel.border = element_blank(), 
                         plot.background = element_rect(fill = "#E6E8Ed"), 
                         axis.line = element_blank(), 
                         axis.text.x = element_blank(), 
                         axis.text.y = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.title.x = element_blank(), 
                         axis.title.y = element_blank()))

p <- ggplot() + 
  make_text(map_flu, -100, 50, "WEEKEND",
            "CDC FluView in Lower 48 States ending %s") + 
  scale_fill_gradient2(low = "white", high = "red", breaks = 0:10,
                       guide = "none") + 
  theme_opts + 
  theme_animint(width = 750, height= 500)

test_that("save separate chunks for geom_polygon", {
  state.map <- p + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = level),
                 data = map_flu, 
                   showSelected = "WEEKEND",                  
                 colour = "black", size = 1)
  viz <-
    list(levelHeatmap = level.heatmap,
         stateMap = state.map,
         title = "FluView")
  out.dir <- file.path(getwd(), "FluView")
  unlink(out.dir, recursive = TRUE)
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  common.chunk <-
    list.files(path = out.dir, pattern = "geom.+polygon.+chunk_common.tsv", 
               full.names = TRUE)
  varied.chunks <-
    list.files(path = out.dir, pattern = "geom.+polygon.+chunk[0-9]+.tsv", 
               full.names = TRUE)
  ## number of chunks
  expect_equal(length(common.chunk), 1L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, length(unique(map_flu$WEEKEND)))
  ## test common.chunk
  common.data <- read.csv(common.chunk, sep = "\t", comment.char = "")
  expect_equal(nrow(common.data), nrow(FluView$USpolygons))
  expect_true(all(c("x", "y", "group") %in% names(common.data)))
  ## randomly choose n varied.chunk to test
  idx <- sample(no.chunks, 1)
  varied.data <- read.csv(varied.chunks[idx], sep = "\t", comment.char = "")
  expect_equal(nrow(varied.data), length(unique(FluView$USpolygons$group)))
  expect_true(all(c("fill", "group") %in% names(varied.data)))
  unlink(out.dir, recursive = TRUE)
})

### test case 2
USdots <-
  ddply(FluView$USpolygons, .(region), summarise,
        mean.lat = mean(lat), 
        mean.long = mean(long))
# add state flu to points.
flu.points <- ldply(unique(state_flu$WEEKEND), function(we) {
  df <- subset(state_flu, WEEKEND == we)
  merge(USdots, df, by.x = "region", by.y = "state")
})

test_that("save separate chunks for geom_point without specifying group", {
  # the compiler will not break a geom into chunks if any of the resulting 
  # chunk tsv files is estimated to be less than 4KB.
  state.map <- p + 
    geom_point(aes(x = mean.long, y = mean.lat, fill = level),
               data = flu.points, 
                   showSelected = "WEEKEND",
               color = "black",
               size = 10)
  viz <-
    list(levelHeatmap = level.heatmap,
         stateMap = state.map,
         title = "FluView")
  out.dir <- file.path(getwd(), "FluView-point")
  unlink(out.dir, recursive = TRUE)
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  common.chunk <-
    list.files(path = out.dir, pattern = "geom.+point.+chunk_common.tsv", 
               full.names = TRUE)
  varied.chunks <-
    list.files(path = out.dir, pattern = "geom.+point.+chunk[0-9]+.tsv", 
        full.names = TRUE)
  ## number of chunks
  expect_equal(length(common.chunk), 0L)
  expect_equal(length(varied.chunks), 1L)
  ## test the only one varied.chunk
  varied.data <- read.csv(varied.chunks, sep = "\t", comment.char = "")
  expect_equal(nrow(varied.data), nrow(flu.points))
  expect_true(all(c("fill", "x", "y", "showSelected1") %in% names(varied.data)))
  unlink(out.dir, recursive = TRUE)
  ## force to split into chunks
  state.map <- p + 
    geom_point(aes(x = mean.long, y = mean.lat, fill = level),
               data = flu.points, 
                   showSelected = "WEEKEND",                
               color = "black",
               size = 10,
               chunk_vars = "WEEKEND",
               validate_params = FALSE)
  viz <-
    list(levelHeatmap = level.heatmap,
         stateMap = state.map,
         title = "FluView")
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  common.chunk <-
    list.files(path = out.dir, pattern = "geom.+point.+chunk_common.tsv", 
               full.names = TRUE)
  varied.chunks <-
    list.files(path = out.dir, pattern = "geom.+point.+chunk[0-9]+.tsv", 
               full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 1L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, length(unique(flu.points$WEEKEND)))
  # test common.chunk
  common.data <- read.csv(common.chunk, sep = "\t", comment.char = "")
  expect_equal(nrow(common.data), nrow(USdots))
  expect_true(all(c("x", "y", "group") %in% names(common.data)))
  # randomly choose an varied.chunk to test
  idx <- sample(no.chunks, 1)
  varied.data <- read.csv(varied.chunks[idx], sep = "\t", comment.char = "")
  expect_equal(nrow(varied.data), nrow(USdots))
  expect_true(all(c("fill", "group") %in% names(varied.data)))
  unlink(out.dir, recursive = TRUE)
})

### test case 3: WorldBank data, without Israel. For some reason
### Israel appears on travis/wercker but not on local computers, so we
### just get rid of it for this test.
data(WorldBank, package = "animint2")

no.israel <- subset(WorldBank, country != "Israel")

## Local computer:

## 77       Europe & Central Asia (all income levels)    85
## 78                                      South Asia    88

## wercker:

## 77 Europe & Central Asia (all income levels) 85
## 78 Middle East & North Africa (all income levels) 86
## 79 South Asia 88

## 77 Ireland population 2932650
## 78 Israel population 2877000
## 79 India population 542983934

life.not.na <- !is.na(no.israel$life.expectancy)
fert.not.na <- !is.na(no.israel$fertility.rate)
pop.not.na <- !is.na(no.israel$population)
text.not.na <- no.israel[life.not.na & fert.not.na, ]
points.not.na <- no.israel[life.not.na & fert.not.na & pop.not.na, ]
unique.year.vec <- unique(points.not.na$year)
unique.country.vec <- unique(no.israel$country)

scatter <- ggplot()+
  geom_point(aes(life.expectancy, fertility.rate,
                 colour=region, size=population,
                 tooltip=paste(country, "population", population),
                 key=country), # key aesthetic for animated transitions!
             clickSelects="country",
             showSelected="year",
             data=no.israel)+
  geom_text(aes(life.expectancy, fertility.rate, label=country,
                key=country), # also use key here!
            data=no.israel,
            showSelected=c("country", "year"),
            chunk_vars=c("year", "country"),
            validate_params = FALSE)+
  scale_size_animint(breaks=10^(5:9))+
  make_text(no.israel, 55, 9, "year")

ts <- ggplot()+
  make_tallrect(no.israel, "year")+
  geom_line(aes(year, life.expectancy, group=country, colour=region),
            data=no.israel, size=4, alpha=3/5,
                clickSelects="country")

test_that("save separate chunks for non-spatial geoms with repetitive field, multiple vars selected, and NAs", {
  viz <-
    list(scatter = scatter,
         ts = ts,
         time=list(variable="year", ms=3000),
         duration=list(year=1000),
         first=list(year=1975, country="United States"),
         title="World Bank data (multiple selections)")
  out.dir <- file.path(getwd(), "WorldBank-all")
  unlink(out.dir, recursive=TRUE)
  info <- animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  ## multiple vars selected
  common.chunk <-
    list.files(path = out.dir, pattern = "geom2_text.+chunk_common.tsv", 
               full.names = TRUE)
  varied.chunks <-
    list.files(path = out.dir, pattern = "geom2_text.+chunk[0-9]+.tsv", 
               full.names = TRUE)
  ## number of chunks
  expect_equal(length(common.chunk), 0L)
  expect_equal(length(varied.chunks), nrow(text.not.na))
  ## choose first varied.chunk to test
  varied.data <- read.csv(varied.chunks[1], sep = "\t", comment.char = "")
  expect_equal(nrow(varied.data), 1)
  expect_true(all(c("x", "y", "label", "key") %in% names(varied.data)))
  ## single var selected
  common.chunk <-
    list.files(path = out.dir, pattern = "geom.+point.+chunk_common.tsv", 
               full.names = TRUE)
  varied.chunks <-
    list.files(path = out.dir, pattern = "geom.+point.+chunk[0-9]+.tsv", 
               full.names = TRUE)
  ## number of chunks
  expect_equal(length(common.chunk), 1L)
  expect_equal(length(varied.chunks), length(unique.year.vec))
  ## test common.chunk
  common.data <- read.csv(common.chunk, sep = "\t", comment.char = "")
  expect_equal(nrow(common.data), length(unique.country.vec))
  common.must.have <- c("colour", "clickSelects", "key", "fill", "group")
  expect_true(all(common.must.have %in% names(common.data)))
  expect_equal(length(grep("showSelected",names(common.data))),1)
  ## choose first varied.chunk to test
  chunk.info <- info$geoms$geom1_point_scatter$chunks
  year.str <- names(chunk.info)[[1]]
  year.num <- as.numeric(year.str)
  expected.data <- subset(points.not.na, year == year.num)
  chunk.num <- chunk.info[[year.str]]
  tsv.name <- sprintf("geom1_point_scatter_chunk%d.tsv", chunk.num)
  tsv.path <- file.path(out.dir, tsv.name)
  varied.data <- read.csv(tsv.path, sep = "\t", comment.char = "")
  expect_equal(nrow(varied.data), nrow(expected.data))
  varied.must.have <-
    c("size", "x", "y", "tooltip", "group")
  expect_true(all(varied.must.have %in% names(varied.data)))
  unlink(out.dir, recursive = TRUE)
})

### test case 4
data(breakpoints, package = "animint2")

only.error <- subset(breakpoints$error, type=="E")
only.segments <- subset(only.error, samples==samples[1])
signal.colors <- c(estimate="#0adb0a", latent="#0098ef")

signal <- ggplot()+
  geom_point(aes(position, signal),
             data=breakpoints$signals, showSelected="samples")+
  geom_line(aes(position, signal), colour=signal.colors[["latent"]],
            data=breakpoints$imprecision)+
  geom_segment(aes(first.base, mean, xend=last.base, yend=mean),
               colour=signal.colors[["estimate"]],
                   showSelected=c("segments", "samples"),
               data=breakpoints$segments)+
  geom_vline(aes(xintercept=base),
             colour=signal.colors[["estimate"]],
                 showSelected=c("segments", "samples"),
             linetype="dashed",
             data=breakpoints$breaks)

test_that("save separate chunks for non-spatial geoms with nest_order not being group", {
  viz <-
    list(signal = signal,
         title="breakpointError (select one model size)")
  out.dir <- file.path(getwd(), "breakpointError-single")
  unlink(out.dir, recursive = TRUE)
  animint2dir(viz, out.dir = out.dir, open.browser = FALSE)
  common.chunk <-
    list.files(path = out.dir, pattern = "geom.+segment.+chunk_common.tsv", 
               full.names = TRUE)
  varied.chunks <-
    list.files(path = out.dir, pattern = "geom.+segment.+chunk[0-9]+.tsv", 
               full.names = TRUE)
  # number of chunks
  expect_equal(length(common.chunk), 1L)
  no.chunks <- length(varied.chunks)
  expect_equal(no.chunks, length(unique(breakpoints$segments$samples)))
  ## test common.chunk
  common.data <- read.csv(common.chunk, sep = "\t", comment.char = "")
  n.samples <- length(unique(breakpoints$segments$samples))
  expected.rows <- nrow(breakpoints$segments) / n.samples
  expect_equal(nrow(common.data), expected.rows)
  common.must.have <- c("showSelected1", "group")
  expect_true(all(common.must.have %in% names(common.data)))
  # randomly choose an varied.chunk to test
  idx <- sample(no.chunks, 1)
  varied.data <- read.csv(varied.chunks[idx], sep = "\t", comment.char = "")
  expect_equal(nrow(varied.data), expected.rows)
  must.have <- c("x", "xend", "y", "yend", "group")
  expect_true(all(must.have %in% names(varied.data)))
  unlink(out.dir, recursive = TRUE)
})
