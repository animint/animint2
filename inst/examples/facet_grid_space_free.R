fake <- data.frame(year=c(1900, 1950, 2000, 2010),
                   y=1,
                   period=c(1, 1, 2, 2))
viz <-
  list(free=a_plot()+
       a_theme_bw()+
       a_theme(panel.margin=grid::unit(0, "cm"))+
       a_geom_point(a_aes(year, y), data=fake)+
       a_scale_x_continuous(breaks=seq(1900, 2010, by=10))+
         a_facet_grid(.~period, scales="free", space="free"))
viz$fixed <- viz$free+a_facet_grid(.~period, scales="free")
viz$freeY <- viz$free+a_facet_grid(.~period, scales="free", space="free_y")
viz$freeX <- viz$free+a_facet_grid(.~period, scales="free", space="free_x")
##viz$noPanels <- viz$free+a_facet_null()
panels <- list()
facets <- list()
space_free <- list()
for(plot.name in names(viz)){
  gg <- viz[[plot.name]]
  built <- a_plot_build(gg)
  panels[[plot.name]] <- built$panel
  facets[[plot.name]] <- gg$a_facet
  space_free[[plot.name]] <- gg$a_facet$space_free
}
str(facets)
## the difference is in space_free:
str(space_free)
animint2dir(viz, "facet-space-free")
