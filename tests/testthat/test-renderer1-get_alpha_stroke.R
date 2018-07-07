acontext("get_alpha_stroke")

  data(state)
  states.data <- as.data.frame(state.x77)
  states.data$states <- rownames(states.data)
  colnames(states.data)[4] <- "Life.Exp"

  viz <- list(geoms_viz = ggplot(data = states.data,
                aes(x = Income, y = Life.Exp)) +
                geom_point(shape = 21,
                            colour = "black",
                            fill = "white",
                            # I include it here to modify the stroke of geom_point
                            # What do you think? Any advice? Is it a fraction the best way to set it?
                            alpha_stroke = 1/10,
                            validate_params = FALSE) +
                ggtitle("Income v. Life Expectancy") +
                theme(panel.background = element_rect(fill = NA),
                      panel.grid.major = element_line(colour = "grey90")
                ))

  info <- animint2HTML(viz)

test_that("alpha_stroke parameter is rendered as stroke-opacity style", {
  opacity.str <- getStyleValue(info$html, "//circle[@class='geom']", "stroke-opacity")
  opacity.num <- as.numeric(opacity.str)
  opacity.exp <- rep(1/10, nrow(states.data))
  expect_equal(opacity.num, opacity.exp)
})


test_that("alpha_stroke parameter is rendered as stroke-opacity style in rects", {
  viz <- list(segs = ggplot() +
                geom_rect(data = df, size = 0.01, color = "violet",
                          alpha_stroke = 1/10,
                          validate_params = FALSE,
                          aes(xmin = xmin, ymin = ymin,
                              xmax = xmax, ymax = ymax)))
  info <- animint2HTML(viz)

opacity.str <- getStyleValue(info$html, "//rect[@class='geom']", "stroke-opacity")
opacity.num <- as.numeric(opacity.str)
opacity.exp <- rep(1/10, nrow(states.data))
expect_equal(opacity.num, opacity.exp)
})


test_that("alpha_stroke parameter is rendered as stroke-opacity style in widerect", {
recommendation <- data.frame(
  min.C=21,
  max.C=23)
set.seed(1)
temp.time <- data.frame(
  time=strptime(paste0("2015-10-", 1:31), "%Y-%m-%d"),
  temp.C=rnorm(31))

viz <- list(
  gg=ggplot()+
    theme_bw()+
    theme_animint(height=200, width=2000)+
    geom_widerect(aes(ymin=min.C, ymax=max.C),
                  color=NA,
                  fill="grey",
                  data=recommendation,
                  alpha_stroke = 1/10,
                  validate_params = FALSE)+
    geom_line(aes(time, temp.C),
              data=temp.time)
  )

info <- animint2HTML(viz)

opacity.str <- getStyleValue(info$html, "//widerect[@class='geom_widerect']", "stroke-opacity")
opacity.num <- as.numeric(opacity.str)
opacity.exp <- rep(1/10, nrow(states.data))
expect_equal(opacity.num, opacity.exp)
})

test_that("alpha_stroke parameter is rendered as stroke-opacity style in tallrect", {

  error.types <- data.frame(x=1:3, status=c("correct", "false positive", "false negative"))

  viz <- list(gg = ggplot(error.types)+
      geom_point(aes(x, x))+
      geom_tallrect(aes(xmin=x, xmax=x+0.5, linetype=status),
                    fill="grey",
                    color="black",
                    alpha_stroke = 1/10,
                    validate_params = FALSE))

  info <- animint2HTML(viz)

opacity.str <- getStyleValue(info$html, "//tallrect[@class='geom_tallrect']", "stroke-opacity")
opacity.num <- as.numeric(opacity.str)
opacity.exp <- rep(1/10, nrow(states.data))
expect_equal(opacity.num, opacity.exp)
})

test_that("alpha_stroke parameter is rendered as stroke-opacity style", {
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

  viz <- list(map=ggplot()+
         geom_polygon(aes(x=long, y=lat, group=group),
                      data=USpolygons,
                      fill="black", 
                      colour="grey",
                      alpha_stroke = 1/10,
                      validate_params = FALSE,) +
         geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                      showSelected="year",
                      colour="#55B1F7", data=UStornadoes))

  info <- animint2HTML(viz)

  opacity.str <- getStyleValue(info$html, "//polygon[@class='geom_polygon']", "stroke-opacity")
  opacity.num <- as.numeric(opacity.str)
  opacity.exp <- rep(1/10, nrow(states.data))
  expect_equal(opacity.num, opacity.exp)
  })
