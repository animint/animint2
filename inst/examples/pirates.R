#' Data Source:
#' National Geospatial Intelligence Agency (NGIA)
#' lat/lon locations and descriptions of pirate attacks 
#' 
#' Data contains the latitude and longitude coordinates 
#' of pirate attacks from 1978-2013.  In some cases, 
#' a description of the attacks is also provided.
#' 
#' See ?animint::pirates for more information

library(animint2)
library(dplyr)      ## for data manipulation
library(tidyr)      ## for data manipulation
library(maps)       ## to plot world map
library(lubridate)  ## to work with dates
library(stringr)    ## to work with strings
a_theme_set(a_theme_bw())

# functions to create 2d bins ---------------------------

# generates the bins needed for 2d binning
bin_2d <- function(x, y, xbins, ybins) {
  
  # binwidths
  x_width <- diff(range(x)) / xbins
  y_width <- diff(range(y)) / ybins
  # cutpoints
  x_cuts <- seq(min(x), max(x), by = x_width)
  y_cuts <- seq(min(y), max(y), by = y_width)
  
  # setting up all the bins
  x_bins <- interaction(x_cuts[-length(x_cuts)], x_cuts[-1], sep = "_")
  y_bins <- interaction(y_cuts[-length(y_cuts)], y_cuts[-1], sep = "_")
  bins <- expand.grid(x_bins, y_bins)
  
  # breaking into columns for x and y lower and upper points
  x_bins2 <- t(sapply(bins$Var1, function(z) str_split(z, "_")[[1]] ))
  y_bins2 <- t(sapply(bins$Var2, function(z) str_split(z, "_")[[1]] ))
  bins <- data.frame(x_low = as.numeric(x_bins2[, 1]), 
                     x_hi = as.numeric(x_bins2[, 2]), 
                     y_low = as.numeric(y_bins2[, 1]), 
                     y_hi = as.numeric(y_bins2[, 2]))
  # identifying mean of each bin
  bins$xmid <- (bins$x_low + bins$x_hi) / 2
  bins$ymid <- (bins$y_low + bins$y_hi) / 2
  bins
}

# counts the number of points in each 2d bin
# double counting points on boundary (not worried about this)
count_bins <- function(bins, x, y) {
  bins$count <- apply(bins, 1, function(z) {
    mean(x >= z[1] & x <= z[2] & y >= z[3] & y <= z[4]) * length(x)
  })
  subset(bins, select = c(xmid, ymid, count))
}

# data -------------------------------------------------

# loading data
data(pirates, package = "animint2")
countries <- map_data("world") %>% 
  filter(region != "Antarctica")

# remove data prior to 1995 - it's not that interesting
p_df <- pirates %>% 
  tbl_df() %>% 
  arrange(DateOfOcc) %>% 
  mutate(date = year(ymd(as.character(DateOfOcc)))) %>% 
  filter(date > 1995)

# aggregating by date
p_df2 <- p_df %>% 
  group_by(date) %>% 
  summarise(attacks = n()) %>% 
  ungroup() %>% 
  mutate(total_attacks = cumsum(attacks))

# binning data --------------------------------------------
# I'm sure that there is a better way to do this

## generating tiles
## each year, I will count the number of attacks in each tile
bin_df <- bin_2d(p_df$coords.x1, p_df$coords.x2, 60, 30) %>% 
  tbl_df() %>% 
  mutate(id = 1:nrow(.)) ## adding a unique id for each bin

# counting attacks in each tile in each year
d <- p_df %>% 
  select(date, coords.x1, coords.x2) %>% 
  group_by(date) %>% 
  do(count = count_bins(bin_df, .$coords.x1, .$coords.x2))

## merging dates and counts
## should figure out how to do this with dplyr
l <- list()
for(i in 1:nrow(d)) {
  l[[i]] <- d$count[[i]]
  l[[i]]$date <- d$date[i]
}

# unnesting the list and counting total attacks
p_df3 <- l %>% 
  unnest() %>% 
  arrange(date, xmid, ymid) %>% 
  group_by(xmid, ymid) %>% 
  mutate(attacks = cumsum(count)) %>% 
  ungroup() %>% 
  filter(attacks > 0) %>% 
  # join with bin_df to add an id column
  inner_join(select(bin_df, xmid, ymid, id))

# ids for each tile
p_df4 <- p_df3 %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  filter(attacks > 0) %>% 
  summarise(text_loc_x = last(date), 
            text_loc_y = last(log(attacks)))

# animint plots -----------------------------------------

# total number of attacks
p_time <- a_plot() + 
  a_geom_line(a_aes(date, total_attacks), data = p_df2) + 
  make_tallrect(p_df2, "date") + 
  labs(y = "Total Attacks", x = "Date", 
       title = "Pirate Attacks from 1995 to 2013") + 
  a_theme_animint(width = 550, height = 350)

# points on world map
p_points <- a_plot() + 
  a_geom_polygon(a_aes(long, lat, group = group), size = I(1), 
               data = countries, fill = "lightgrey", colour = "darkgreen") +
  a_geom_point(a_aes(coords.x1, coords.x2),
             showSelected = "date", 
             size = 3, alpha = I(.5), data = p_df) + 
  make_text(p_df, 0, 90, "date", "Pirate Attacks in %d") + 
  a_theme(panel.background = a_element_rect(fill = "lightblue"), 
        axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank(), 
        panel.grid.major=a_element_blank(), panel.grid.minor=a_element_blank()) + 
  a_theme_animint(width = 550, height = 350)

# tiles on world map
p_tiles <- a_plot() + 
  a_geom_polygon(a_aes(long, lat, group = group), size = I(1), 
               data = countries, fill = "lightgrey", colour = "darkgreen") +
  a_geom_tile(a_aes(xmid, ymid, fill = log(attacks)), 
            showSelected = "date", clickSelects = "id", 
            data = p_df3, colour = I("red")) + 
  a_geom_text(a_aes(xmid, ymid, a_label = id),
            showSelected = "id", 
            data = p_df3) + 
  make_text(p_df, 0, 90, "date", "Pirate Attacks from 1995 to %d") + 
  a_scale_fill_gradient(low = "#fee5d9", high = "#a50f15", name = "Attacks", 
                      a_labels = c(1, 10, 50, 400), 
                      breaks = log(c(1, 10, 50, 400))) + 
  a_theme(panel.background = a_element_rect(fill = "lightblue"), 
        axis.line=a_element_blank(), axis.text=a_element_blank(), 
        axis.ticks=a_element_blank(), axis.title=a_element_blank(), 
        panel.grid.major=a_element_blank(), panel.grid.minor=a_element_blank()) + 
  a_theme_animint(width = 550, height = 350)

# tiles over time
p_time2 <- a_plot() + 
  make_tallrect(p_df2, "date") + 
  a_geom_line(a_aes(date, log(attacks), group = id), 
            clickSelects = "id", showSelected = "id", 
            data = p_df3) + 
  a_geom_text(a_aes(text_loc_x, text_loc_y, a_label = id), 
            clickSelects = "id", showSelected = "id", 
            colour = "red", data = p_df4) + 
  a_scale_y_continuous(a_labels = c(1, 7, 55, 400), name = "Total Attacks") + 
  a_scale_x_continuous(breaks = c(1995, 2000, 2005, 2010), name = "Date", limits = c(1995, 2013)) + 
  a_theme_animint(height = 350, width = 550) + 
  ggtitle("Attacks in Individual Tiles")

# passing to animint
viz <- list( 
  points = p_points, 
  tiles = p_tiles, 
  total = p_time, 
  tileTotal = p_time2,  
  time = list(variable = "date", ms = 300), 
  selector.types = list(id = "multiple"),
  first = list(id = 767), 
  title = "Pirates Example"
)
animint2dir(viz, "pirates_viz", open.browser = FALSE)
servr::httd("pirates_viz")

animint2gist(viz, "Pirate Attacks Since 1995")
