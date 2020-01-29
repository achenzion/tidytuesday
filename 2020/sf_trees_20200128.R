library(ggmap)
library(dplyr)
library(tidyquant)
library(scales) 
library(tidyr)
library(ggpubr)

# Read in data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')  %>%
  mutate(date = replace_na(date, "1955-01-01"))
# Fill in missing dates with latest possible date

# Pick a date of interest.
# TO DO: Make this script into a shiny app and have this be a selector
up_to_date <- "1980-01-01"

#### Visualize trees on map that were planted before the selected date

# Filter data
sf_visual_df <- sf_trees %>% filter(date<up_to_date)

# Get base map
# Requires Google Map API Access see:
# https://www.rdocumentation.org/packages/ggmap/versions/3.0.0
center <- c(lon = mean(sf_trees$longitude,na.rm=TRUE), lat = mean(sf_trees$latitude,na.rm=TRUE))
sf_map <- qmap(center, zoom = 12, source = "stamen", maptype = "terrain-lines")

#plot args
map_title <- paste("Distribution of Trees in SF on",up_to_date)

# Construct tree map
tree_map <- sf_map  +
  stat_density2d(
    aes(x = longitude, y = latitude, fill = ..level..),
    size = 0.01, bins = 30, data = sf_visual_df,
    geom = "polygon", alpha=0.1
  ) +
  scale_fill_distiller(palette = 'Greens', trans = "reverse") + 
  ggtitle(map_title) + 
  theme(plot.title = element_text(vjust=0.5,hjust=0.5))

#### Plot Cumulative Number of Trees with vertical line at selected date

# Calculate cumulative number of trees
sf_cumsum_trees <- sf_trees %>% select("tree_id","date") %>% 
                             arrange(date) %>%
                             group_by(date) %>% tally() %>%
                             mutate(total_trees = cumsum(n))

# Plot arguments
trees <- nrow(sf_trees) # Number of trees for axis max
scaleFUN <- function(x) sprintf("%.0fK", x/1000) # custom formatting function

# Plot
tree_total <- ggplot(data=sf_cumsum_trees, aes(x=as.POSIXct(date), y=total_trees,color="green")) +
  geom_line(color="green") + scale_y_continuous(limits = c(0,trees), 
                                   labels = scaleFUN) + 
  geom_vline(xintercept = as.POSIXct(up_to_date),color="brown") +
  xlab("Date") + ylab("Total trees (K)") +
  ggtitle("Total trees planted over time") + 
  theme(plot.title = element_text(vjust=0.5,hjust=0.5))

sf_tree_fig <- ggarrange(tree_total, tree_map,
                    labels = c("", ""),
                    ncol = 2, nrow = 1)

ggsave(filename="sf_trees_20200128.jpg", width=8, height=3, plot=sf_tree_fig)
