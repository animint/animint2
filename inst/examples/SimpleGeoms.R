#' Tests for each geom
library(plyr)
library(animint2)

#' abline: should show two lines: one running through the points, the other with an intercept of 0 and slope of 2.
xydata <- data.frame(x=sort(runif(50, 0, 10)))
xydata$y <- 3+2*xydata$x + rnorm(50, 0, 1)
g1 <- a_plot() + a_geom_point(data=xydata, a_aes(x=x, y=y)) + 
  a_geom_abline(data=data.frame(intercept=c(3, 0), slope=c(2,1)), a_aes(intercept=intercept, slope=slope)) +
  ggtitle("a_geom_abline")
g1
# gg2animint(list(g1=g1))

#' ribbon: should show two overlapping ribbons, with the same basic shape, one translated up by one unit.
ribbondata <- data.frame(x=seq(0, 1, .1), ymin=runif(11, 0, 1), ymax=runif(11, 1, 2))
ribbondata <- rbind(cbind(ribbondata, group="low"), cbind(ribbondata, group="high"))
ribbondata[12:22,2:3] <- ribbondata[12:22,2:3]+1
g2 <- a_plot() + 
  a_geom_ribbon(data=ribbondata, a_aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=group), alpha=.5) + 
  ggtitle("a_geom_ribbon")
g2
# gg2animint(list(g1=g1, g2=g2))

#' density: should show two normal distributions, centered at 0 and 3, and a gamma distribution with mode approximately 5
boxplotdata <- rbind(data.frame(x=1:50, y=sort(rnorm(50, 3, 1)), group="N(3,1)"),
                     data.frame(x=1:50, y=sort(rnorm(50, 0, 1)), group="N(0,1)"), 
                     data.frame(x=1:50, y=sort(rgamma(50, 2, 1/3)), group="Gamma(2,1/3)"))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax=max(y), ymin=min(y), med=median(y))

g3 <- a_plot() + a_geom_density(data=boxplotdata, a_aes(x=y, group=group, fill=group), alpha=.5) +
  a_scale_fill_discrete("Distribution") + xlab("x") + 
  ggtitle("a_geom_density")
g3
# gg2animint(list(g1=g1, g2=g2, g3=g3))


#' tile: should show an approximately bivariate normal distribution.
tiledata <- data.frame(x=rnorm(1000, 0, 3))
tiledata$y <- rnorm(1000, tiledata$x, 3)
tiledata$rx <- round(tiledata$x)
tiledata$ry <- round(tiledata$y)
tiledata <- ddply(tiledata, .(rx,ry), summarise, n=length(rx))

g4 <- a_plot() + a_geom_tile(data=tiledata, a_aes(x=rx, y=ry, fill=n)) +
  a_scale_fill_gradient(low="#56B1F7", high="#132B43") + 
  xlab("x") + ylab("y") + ggtitle("a_geom_tile")
g4
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4))

#' path: should show a two-dimensional random walk, where x and y are position, z is time.
pathdata <- data.frame(x=rnorm(30, 0, .5), y=rnorm(30, 0, .5), z=1:30)
g5 <- a_plot() + a_geom_path(data=pathdata, a_aes(x=x, y=y), alpha=.5) +
  a_geom_text(data=pathdata, a_aes(x=x, y=y, a_label=z)) + 
  ggtitle("a_geom_path")
g5
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5))

#' Polygons
polydata <- rbind(
  data.frame(x=c(0, .5, 1, .5, 0), y=c(0, 0, 1, 1, 0), group="parallelogram", fill="blue", xc=.5, yc=.5),
  data.frame(x=c(.5, .75, 1, .5), y=c(.5, 0, .5, .5), group="triangle", fill="red", xc=.75, yc=.33)
  )
g6 <- a_plot() + 
  a_geom_polygon(data=polydata, a_aes(x=x, y=y, group=group, fill=fill, colour=fill), alpha=.5)+
  a_scale_colour_identity() + a_scale_fill_identity()+
  a_geom_text(data=polydata, a_aes(x=xc, y=yc, a_label=group)) +
  ggtitle("a_geom_polygon")
g6
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6))

#' Boxplots
#' Boxplot does not work (7/5/13)
# g7 <- a_plot() + 
#   a_geom_boxplot(data=boxplotdata, a_aes(y=y, x=factor(group))) +
#   ggtitle("a_geom_boxplot")
# g7
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7))

g7 <- a_plot() + 
  a_geom_linerange(data=boxplotdata, a_aes(x=factor(group), ymax=ymax, ymin=ymin, colour=factor(group))) +
  ggtitle("a_geom_linerange") + a_scale_colour_discrete("Distribution") + xlab("Distribution")
g7
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7))

g8 <- a_plot() + 
  a_geom_histogram(data=subset(boxplotdata, group=="Gamma(2,1/3)"), a_aes(x=y, fill=..count..), binwidth=1) + 
  ggtitle("a_geom_histogram")
g8
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8))

g9 <- a_plot() + 
  a_geom_violin(data=boxplotdata, a_aes(x=group, y=y, fill=group, group=group)) +
  ggtitle("a_geom_violin")+ a_scale_fill_discrete("Distribution") + xlab("Distribution")
g9
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9))

#' Step Plot
#' Must specify group and then use colour=factor(group) to get desired effect.
g10 <- a_plot() + a_geom_step(data=boxplotdata, a_aes(x=x, y=y, colour=factor(group), group=group)) +
  a_scale_colour_discrete("Distribution") +
  ggtitle("a_geom_step")
g10
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10))

#' contour plot
library(reshape2) # for melt
contourdata <- melt(volcano)
names(contourdata) <- c("x", "y", "z")
g11 <- a_plot() + a_geom_contour(data=contourdata, a_aes(x=x, y=y, z=z), binwidth=4, size=0.5) + 
  a_geom_contour(data=contourdata, a_aes(x=x, y=y, z=z), binwidth=10, size=1) +
  ggtitle("a_geom_contour")
g11
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11))

contourdata2 <- floor(contourdata/3)*3
g12 <- a_plot() + 
  a_geom_tile(data=contourdata2, a_aes(x=x, y=y, fill=z, colour=z)) + 
  a_geom_contour(data=contourdata, a_aes(x=x, y=y, z=z), colour="black", size=.5) +
  a_scale_fill_continuous("height", low="#56B1F7", high="#132B43", a_guide="legend") +
  a_scale_colour_continuous("height", low="#56B1F7", high="#132B43", a_guide="legend") +
  ggtitle("a_geom_tile + a_geom_contour") 
g12
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12)) 

library("MASS")
data(geyser,package="MASS")
g13 <- a_plot() +  
  a_geom_point(data=geyser, a_aes(x = duration, y = waiting)) + 
  a_geom_contour(data=geyser, a_aes(x = duration, y = waiting), colour="blue", size=.5, a_stat="density2d") + 
  xlim(0.5, 6) + a_scale_y_log10(limits=c(40,110)) +
  ggtitle("a_geom_contour 2d density")
g13
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13))

g14 <- a_plot() +  
  a_geom_polygon(data=geyser,a_aes(x=duration, y=waiting, fill=..level.., 
                               group=..piece..), 
               a_stat="density2d", alpha=.5) +
  a_geom_point(data=geyser, a_aes(x = duration, y = waiting)) + 
  a_scale_fill_continuous("Density Level", low="#56B1F7", high="#132B43") + 
  a_guides(colour = a_guide_legend(override.a_aes = list(alpha = 1)), 
         fill = a_guide_legend(override.a_aes = list(alpha = 1))) + 
  a_scale_y_continuous(limits=c(40,110), trans="log10") +
  a_scale_x_continuous(limits=c(.5, 6)) +
  ggtitle("a_geom_density2d polygon")
g14
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14))


data(diamonds)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
g15 <- a_plot() + 
  a_geom_tile(data=dsmall, a_aes(x=carat, y=price, fill=..density.., colour=..density..), a_stat="density2d", contour=FALSE, n=30) +
  a_scale_fill_gradient(limits=c(1e-5,8e-4), na.value="white") + 
  a_scale_colour_gradient(limits=c(1e-5,8e-4), na.value="white") +
  ggtitle("a_geom_density2d tile") + ylim(c(0, 19000))
g15
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15))
  
g16 <- a_plot() + 
  a_geom_point(data=dsmall, a_aes(x=carat, y=price, alpha=..density..), 
             a_stat="density2d", contour=FALSE, n=10, size=I(1)) +
  a_scale_alpha_continuous("Density") +
  ggtitle("a_geom_density2d points")
g16
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, g16=g16))


#' a_geom_map using a_geom_polygon and merge
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
library(reshape2) # for melt
crimesm <- melt(crimes, id = 1)
library(maps)
states_map <- map_data("state")
assault.map <- merge(states_map, subset(crimesm, variable=="Assault"), by.x="region", by.y="state")
assault.map <- assault.map[order(assault.map$group, assault.map$order),]
g17 <- a_plot() + 
  a_geom_polygon(data=assault.map, a_aes(x=long, y=lat, group=group, fill=value, colour=value)) +
  expand_limits(x = states_map$long, y = states_map$lat) + 
  ggtitle("a_geom_polygon map") + ylim(c(12, 63)) + 
  a_geom_text(data=data.frame(x=-95.84, y=55, a_label="Arrests for Assault"), hjust=.5, a_aes(x=x, y=y, a_label=a_label))
g17  
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17))

#' a_geom_bar stacked
data(mtcars)
g18 <- a_plot() + a_geom_bar(data=mtcars, a_aes(x=factor(cyl), fill=factor(vs))) + ggtitle("a_geom_bar stacked")
g18
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17, g18=g18))

#' a_geom_area
data(diamonds)
g19 <- a_plot() + 
  a_geom_area(data=diamonds, a_aes(x=clarity, y=..count.., group=cut, colour=cut, fill=cut), a_stat="density") +
  ggtitle("a_geom_area")
g19
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17, g18=g18, g19=g19))

g20 <- a_plot() + 
  a_geom_freqpoly(data=diamonds, a_aes(x=clarity, group=cut, colour=cut)) +
  ggtitle("a_geom_freqpoly")
g20
# gg2animint(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
#                 g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
#                 g16 = g16, g17=g17, g18=g18, g19=g19, g20=g20))

g21 <- a_plot() + 
  a_geom_hex(data=dsmall, a_aes(x=carat, y=price)) +
  a_scale_fill_gradient(low="#56B1F7", high="#132B43") + 
  xlab("x") + ylab("y") + ggtitle("a_geom_hex")
animint2dir(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, 
                g9=g9, g10=g10, g11=g11, g12=g12, g13=g13, g14=g14, g15=g15, 
                g16 = g16, g17=g17, g18=g18, g19=g19, g20=g20, g21=g21))
