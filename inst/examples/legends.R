library(animint2)
library(plyr) # to access round_any
movies$decade <- round_any(movies$year, 10)
m <- a_plot(movies, aes(x=rating, colour=decade, group=decade)) + 
  geom_density(fill=NA) + scale_colour_continuous(guide="legend") 

m <- a_plot(movies, aes(x=rating, colour=decade, group=decade)) + geom_density(fill=NA) #+ scale_colour_continuous(guide="none")
m 

mb <- a_plot_build(m)
aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))

getLegendList <- function(mb){
  aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))
  lapply(aes.scales, getLegend, mb)
}


getLegend <- function(mb, i){
  sc <- mb$plot$scales$scales[[i]]
  guidetype <- sc$guide
  sc.aes <- sc$aesthetics
  bk <- scale_breaks(sc)
  val <- scale_map(sc, bk)
  labels <- scale_labels(sc)
  if(sc.aes %in% c("colour", "fill")){
    val <- toRGB(val)
  }
  df <- data.frame(breaks = bk, value = val, label = labels)
  df <- df[which(rowSums(is.na(df))==0),] # return only those entries that have breaks, values, and labels.
  if(guidetype=="none"){
    NULL
  } else{
    list(guide = guidetype, 
         aesthetic = sc.aes, 
         title = as.character(as.expression(mb$plot$mapping[[sc.aes]])), 
         legend = df)
  }
}
legends <- lapply(aes.scales, getLegend, mb=mb)

m <- a_plot(movies, aes(x=length, y=rating, size=votes, colour=factor(Comedy))) + scale_colour_manual(values=c("black", "green")) +
  geom_jitter(alpha=.5) + scale_size_area() + xlim(c(20, 300))
m
mb <- a_plot_build(m)
aes.scales <- which(sapply(mb$plot$scales$scales,
                           function(i) sum(i$aesthetics%in%c("colour", "size",
                                                             "fill", "linetype", "alpha"))>0))

legends <- lapply(aes.scales, getLegend, mb=mb)



#------------------
data <- a_plot_build(p)


gdefs <- guides_train(scales = scales,
                      theme = theme,
                      guides = guides,
                      labels = labels)
if (length(gdefs) == 0) return(zeroGrob())
gdefs <- guides_merge(gdefs)
gdefs

getLegend <- function(mb){
  guidetype <- mb$name
  sc.aes <- names(mb$key)[which(substr(names(mb$key), 1, 1)!=".")]
  val <- mb$key[[sc.aes]]
  labels <- mb$key[[".label"]]
  key <- mb$key
  if("colour"%in%sc.aes){
    key[["colour"]] <- toRGB(key$colour)
  }
  if("fill"%in%sc.aes){
    key[["fill"]] <- toRGB(key$fill)
  }
  entries <- key
  if(guidetype=="none"){
    NULL
  } else{
    list(guide = guidetype, 
         aesthetic = sc.aes, 
         title = mb$title, 
         entries = lapply(1:nrow(entries), function(i) as.list(entries[i,])))
  }
}