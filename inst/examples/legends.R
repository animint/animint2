library(animint2)
library(plyr) # to access round_any
movies$decade <- round_any(movies$year, 10)
m <- a_plot(movies, a_aes(x=rating, colour=decade, group=decade)) + 
  a_geom_density(fill=NA) + a_scale_colour_continuous(a_guide="legend") 

m <- a_plot(movies, a_aes(x=rating, colour=decade, group=decade)) + a_geom_density(fill=NA) #+ a_scale_colour_continuous(a_guide="none")
m 

mb <- a_plot_build(m)
a_aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))

getLegendList <- function(mb){
  a_aes.scales <- which(sapply(mb$plot$scales$scales, function(i) sum(i$aesthetics%in%c("colour", "size", "fill", "linetype", "alpha"))>0))
  lapply(a_aes.scales, getLegend, mb)
}


getLegend <- function(mb, i){
  sc <- mb$plot$scales$scales[[i]]
  a_guidetype <- sc$a_guide
  sc.a_aes <- sc$aesthetics
  bk <- scale_breaks(sc)
  val <- scale_map(sc, bk)
  a_labels <- scale_labels(sc)
  if(sc.a_aes %in% c("colour", "fill")){
    val <- toRGB(val)
  }
  df <- data.frame(breaks = bk, value = val, a_label = a_labels)
  df <- df[which(rowSums(is.na(df))==0),] # return only those entries that have breaks, values, and a_labels.
  if(a_guidetype=="none"){
    NULL
  } else{
    list(a_guide = a_guidetype, 
         aesthetic = sc.a_aes, 
         title = as.character(as.expression(mb$plot$mapping[[sc.a_aes]])), 
         legend = df)
  }
}
legends <- lapply(a_aes.scales, getLegend, mb=mb)

m <- a_plot(movies, a_aes(x=length, y=rating, size=votes, colour=factor(Comedy))) + a_scale_colour_manual(values=c("black", "green")) +
  a_geom_jitter(alpha=.5) + a_scale_size_area() + xlim(c(20, 300))
m
mb <- a_plot_build(m)
a_aes.scales <- which(sapply(mb$plot$scales$scales,
                           function(i) sum(i$aesthetics%in%c("colour", "size",
                                                             "fill", "linetype", "alpha"))>0))

legends <- lapply(a_aes.scales, getLegend, mb=mb)



#------------------
data <- a_plot_build(p)


gdefs <- a_guides_train(scales = scales,
                      theme = theme,
                      a_guides = a_guides,
                      a_labels = a_labels)
if (length(gdefs) == 0) return(a_zeroGrob())
gdefs <- a_guides_merge(gdefs)
gdefs

getLegend <- function(mb){
  a_guidetype <- mb$name
  sc.a_aes <- names(mb$key)[which(substr(names(mb$key), 1, 1)!=".")]
  val <- mb$key[[sc.a_aes]]
  a_labels <- mb$key[[".a_label"]]
  key <- mb$key
  if("colour"%in%sc.a_aes){
    key[["colour"]] <- toRGB(key$colour)
  }
  if("fill"%in%sc.a_aes){
    key[["fill"]] <- toRGB(key$fill)
  }
  entries <- key
  if(a_guidetype=="none"){
    NULL
  } else{
    list(a_guide = a_guidetype, 
         aesthetic = sc.a_aes, 
         title = mb$title, 
         entries = lapply(1:nrow(entries), function(i) as.list(entries[i,])))
  }
}