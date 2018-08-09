library(animint2)
centers <- expand.grid(x=1:10, y=1:20)
aligned <- a_plot(centers)+
  a_geom_rect(a_aes(xmin=x-1/2, ymin=y-1/2,
                xmax=x+1/2, ymax=y+1/2),
            fill="white", colour="black")+
  a_geom_text(a_aes(x, y, a_label=sprintf("x=%d,y=%d", x, y)),
            vjust=1/2)
print(aligned)
animint2dir(list(aligned=aligned), "aligned")
