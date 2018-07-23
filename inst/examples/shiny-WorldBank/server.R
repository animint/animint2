library(shiny)
library(animint2)
data(WorldBank)
WorldBank$literacy <- WorldBank[["15.to.25.yr.female.literacy"]]
WorldBank$latitude <- as.numeric(paste(WorldBank$latitude))
WorldBank$longitude <- as.numeric(paste(WorldBank$longitude))
is.discrete <- function(x){
  is.factor(x) || is.character(x) || is.logical(x)
}

shinyServer(function(input, output) {
  
  getViz <- reactive({
    BOTH <- function(df, top, side){
      data.frame(df,
                 top=factor(top, c(input$x, "Years")),
                 side=factor(side, c("Years", input$y)))
    }
    TS <- function(df)BOTH(df, "Years", input$y)
    SCATTER <- function(df)BOTH(df, input$x, input$y)
    TS2 <- function(df)BOTH(df, input$x, "Years")
    y.na <- WorldBank[[input$y]]
    x.na <- WorldBank[[input$x]]
    not.na <- WorldBank[!(is.na(y.na) | is.na(x.na)),]
    not.na.x <- WorldBank[!is.na(x.na),]
    not.na.x$year.country <- with(not.na.x, paste(year, country))
    not.na.y <- WorldBank[!is.na(y.na),]
    not.na.y$year.country <- with(not.na.y, paste(year, country))
    x <- not.na.x[[input$x]]
    y <- not.na.y[[input$y]]
    years <- unique(WorldBank[, "year", drop=FALSE])
    years$x <- (max(x)+min(x))/2
    years$y <- max(y)
    by.country <- split(not.na, not.na$country)
    min.years <- do.call(rbind, lapply(by.country, subset, year == min(year)))
    min.years$year <- 1958
    data_i <-  SCATTER(not.na)
    data_i$color <- input$color
    
    gg <-
      a_plot()+
        a_theme_bw()+
        a_theme(panel.margin=grid::unit(0, "lines"))+
        xlab("")+
        ylab("")+
        a_geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2),
                      clickSelects="year",
                      data=TS(years), alpha=1/2)+
        a_theme_animint(width=1000, height=800)+
        a_geom_line(aes_string(
            x="year",
            y=input$y,
            group="country",
            colour=input$color,
            key="country"),
          clickSelects="country",
          data=TS(not.na.y), size=4, alpha=3/5)+
        a_geom_point(aes_string(
            x="year",
            y=input$y,
            color=input$color,
            size=input$size,
            key="year.country"),
          showSelected="country",
          clickSelects="country",
          data=TS(not.na.y))+
        a_geom_text(aes_string(
            x="year",
            y=input$y,
            key="country",
            colour=input$color,
            label="country"),
          showSelected="country",
          clickSelects="country",
          data=TS(min.years), hjust=1)+
        a_geom_widerect(aes(ymin=year-1/2, ymax=year+1/2),
                      clickSelects="year",
                      data=TS2(years), alpha=1/2)+
        a_geom_path(aes_string(
            x=input$x,
            y="year",
            group="country",
            colour=input$color,
            key="country"),
          clickSelects="country",
          data=TS2(not.na.x), size=4, alpha=3/5)+
        a_geom_point(aes_string(
            x=input$x,
            y="year",
            color=input$color,
            size=input$size,
            key="year.country"),
          showSelected="country",
          clickSelects="country",
          data=TS2(not.na.x))+
        a_geom_point(aes_string(
            x=input$x,
            y=input$y,
            id="country",
            colour=input$color,
            size=input$size,
            key="country"),
          showSelected="year",
          clickSelects="country",
          data=SCATTER(not.na))+
        a_geom_text(aes_string(
            x=input$x,
            y=input$y,
            label="country",
            key="country"),
          showSelected=c("country","year", "color"),
          clickSelects="country", 
          data=data_i)+
        a_facet_grid(side ~ top, scales="free")+
        a_geom_text(aes(x, y,
          label=paste0("year = ", year)),
          showSelected="year",
          data=SCATTER(years))
      if(is.discrete(not.na[[input$size]])){
        gg <- gg+a_scale_size_discrete()
      }else{
        gg <- gg+a_scale_size_animint()
      }
    list(ts=gg,
         time=list(variable="year",ms=2000),
         duration=list(year=1000),
         first=list(year=1975, country=c("United States", "Vietnam")),
         selector.types=list(country="multiple"),
         title="World Bank data (multiple selection, facets)")
  })
  
  output$animint <- renderAnimint({
    # unlike plotOutput, height/width is controlled with a_theme_animint()
    getViz()
  })
  
})
