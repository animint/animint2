if(require('data.table'))setDTthreads(1)#for CRAN.
library(animint2)
## Simple hello world example (1 selector: word).
animint(ggplot()+geom_text(aes(
  word, word, label=word, color=word),
  data=data.frame(word=c("Hello","world!"))))

## More complex Hello World (2 selectors: number, language).
hello_df <- data.frame(
  language=c("en","en","fr","fr"),
  word=c("Hello","world!","Bonjour","monde !"),
  number=factor(c(1,2,1,2)))
lang_df <- data.frame(number=factor(1:2), language=c("en","fr"))
animint(
  ggplot()+
    geom_text(aes(
      number, "message", label=word, color=number),
      showSelected="language", data=hello_df)+
    geom_text(aes(
      number, "select language", label=language),
      clickSelects="language",
      data=lang_df))

## More complex World Bank example (3 selectors: country, region, year).
data(WorldBank, package="animint2")
WorldBank$Region <- sub(" (all income levels)", "", WorldBank$region, fixed=TRUE)
years <- unique(WorldBank[, "year", drop=FALSE])
y1960 <- subset(WorldBank, year==1960)
animint(
  title="Linked scatterplot and time series", #web page title.
  time=list(variable="year",ms=3000), #variable and time delay used for animation.
  duration=list(country=1000, year=1000), #smooth transition duration in milliseconds.
  selector.types=list(country="multiple"), #single/multiple selection for each variable.
  first=list( #selected values to show when viz is first rendered.
    country=c("Canada", "Japan"),
    year=1970),
  ## ggplots are rendered together for an interactive data viz.
  ts=ggplot()+
    theme_animint(width=500)+
    theme(legend.position="none")+
    make_tallrect(WorldBank, "year")+
    geom_label_aligned(aes(
      year, life.expectancy,
      key=country,
      color=Region,
      label=country),
      showSelected=c("country","Region"),
      clickSelects="country",
      hjust=1,
      data=y1960)+
    scale_x_continuous(
      breaks=seq(1960, 2010, by=10),
      limits=c(1940, NA))+
    geom_line(aes(
      year, life.expectancy, group=country, color=Region),
      clickSelects="country",
      showSelected="Region",
      data=WorldBank,
      size=4,
      alpha=0.55),
  scatter=ggplot()+
    geom_point(aes(
      fertility.rate, life.expectancy,
      key=country, colour=Region, size=population),
      chunk_vars=character(),
      showSelected="year",
      clickSelects="country",
      data=WorldBank)+
    geom_text(aes(
      fertility.rate, life.expectancy,
      key=country,
      label=country),
      showSelected=c("country", "year"),
      chunk_vars=character(),
      data=WorldBank)+
    geom_text(aes(
      5, 80, key=1, label=paste("year =", year)),
      showSelected="year",
      data=years)+
    scale_x_continuous(breaks=1:9)+
    scale_size_animint(pixel.range=c(2,20), breaks=10^(4:9)))
