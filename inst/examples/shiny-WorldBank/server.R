library(shiny)
library(animint2)
library(maps)  # Add this line
data(WorldBank)
WorldBank$literacy <- WorldBank[["15.to.25.yr.female.literacy"]]
WorldBank$latitude <- as.numeric(paste(WorldBank$latitude))
WorldBank$longitude <- as.numeric(paste(WorldBank$longitude))


# Map data processing (from first code)
map_df <- animint2::map_data("world")
country2region <- with(unique(WorldBank[, c("region","country")]), structure(region, names=country))
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
  Iran="Iran, Islamic Rep.",
  Kyrgyzstan="Kyrgyz Republic",
  "Saint Kitts"="St. Kitts and Nevis", 
  "North Korea"="Korea, Dem. Rep.",
  "South Korea"="Korea, Rep.",
  Laos="Lao PDR",
  "Saint Lucia"="St. Lucia",
  "North Macedonia"="Macedonia, FYR", 
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
map_disp$region <- country2region[map_disp$country]

is.discrete <- function(x){
  is.factor(x) || is.character(x) || is.logical(x)
}

# server.R
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
    MAP <- function(df)BOTH(df, "Years", "Years")  # Add MAP function
    
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
    
    # Process map coordinates (from first code)
    first.year <- min(WorldBank$year, na.rm=TRUE)
    last.year <- max(WorldBank$year, na.rm=TRUE)
    map_names <- c(x="long", y="lat")
    for(new.var in names(map_names)){
      old.var <- map_names[[new.var]]
      old.val <- map_df[[old.var]]
      m <- min(old.val)
      old.01 <- (old.val-m)/(max(old.val)-m)
      map_disp[[new.var]] <- old.01*(last.year-first.year)+first.year
    }
    
    gg <-
      ggplot()+
        theme_bw()+
        theme(panel.margin=grid::unit(0, "lines"))+
        xlab("")+
        ylab("")+
        geom_tallrect(aes(xmin=year-1/2, xmax=year+1/2),
                      clickSelects="year",
                      data=TS(years), alpha=1/2)+
        theme_animint(width=1000, height=800)+
        geom_line(aes_string(
            x="year",
            y=input$y,
            group="country",
            colour=input$color,
            key="country"),
          clickSelects="country",
          data=TS(not.na.y), size=4, alpha=3/5)+
        geom_point(aes_string(
            x="year",
            y=input$y,
            color=input$color,
            size=input$size,
            key="year.country"),
          showSelected="country",
          clickSelects="country",
          data=TS(not.na.y))+
        geom_text(aes_string(
            x="year",
            y=input$y,
            key="country",
            colour=input$color,
            label="country"),
          showSelected="country",
          clickSelects="country",
          data=TS(min.years), hjust=1)+
        geom_widerect(aes(ymin=year-1/2, ymax=year+1/2),
                      clickSelects="year",
                      data=TS2(years), alpha=1/2)+
        geom_path(aes_string(
            x=input$x,
            y="year",
            group="country",
            colour=input$color,
            key="country"),
          clickSelects="country",
          data=TS2(not.na.x), size=4, alpha=3/5)+
        geom_point(aes_string(
            x=input$x,
            y="year",
            color=input$color,
            size=input$size,
            key="year.country"),
          showSelected="country",
          clickSelects="country",
          data=TS2(not.na.x))+
        geom_point(aes_string(
            x=input$x,
            y=input$y,
            id="country",
            colour=input$color,
            size=input$size,
            key="country"),
          showSelected="year",
          clickSelects="country",
          data=SCATTER(not.na))+
        geom_text(aes_string(
            x=input$x,
            y=input$y,
            label="country",
            key="country"),
          showSelected=c("country","year", "color"),
          clickSelects="country", 
          data=data_i)+
        # Add world map polygon (from first code)
        geom_polygon(aes(
            x, y, group=group, fill=region),
          title="World map",
          clickSelects="country",
          color="black",
          color_off="transparent",
          alpha=1,
          alpha_off=0.3,
          data=MAP(map_disp))+
        facet_grid(side ~ top, scales="free")+
        geom_text(aes(x, y,
          label=paste0("year = ", year)),
          showSelected="year",
          data=SCATTER(years))
          
      if(is.discrete(not.na[[input$size]])){
        gg <- gg+scale_size_discrete()
      }else{
        gg <- gg+scale_size_animint()
      }
    list(ts=gg,
         time=list(variable="year",ms=2000),
         duration=list(year=1000),
         first=list(year=1975, country=c("United States", "Vietnam")),
         selector.types=list(country="multiple"),
         title="World Bank data (multiple selection, facets)")
  })
  
  output$animint <- renderAnimint({
    # unlike plotOutput, height/width is controlled with theme_animint()
    getViz()
  })
  
})