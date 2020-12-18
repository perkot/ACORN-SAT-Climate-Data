# ----------------------------------
# DEPENDENCIES 
# ----------------------------------

library(tidyverse)
library(forcats)

# ----------------------------------
# DATA-SOURCE
# ----------------------------------

tbl <- read.csv("/Users/perkot/Dropbox/Analytics/Personal/R/git/ACORN-SAT-Climate-Data-gganimate/ACORN-SAT-Clean.csv")

# ----------------------------------
# TIDY STATION NAMES 
# ----------------------------------

# Sub-out only first word 
# "([A-Za-z]+).*", "\\1" 

# Parse out first two-words of string
tbl$Station <- gsub("(\\w+\\s+\\w+).*", "\\1", 
                    tbl$site.name)

# Get unique station names 
unique_station <- unique(tbl$Station)
unique_station

# Remove these specific words 
tbl <- tbl %>% mutate_all(~gsub("AIRPORT", "", .))
tbl <- tbl %>% mutate_all(~gsub("AERO", "", .))
tbl <- tbl %>% mutate_all(~gsub("RAAF", "", .))

# Change these station names 
tbl$Station[tbl$Station == "BATHURST AGRICULTURAL"] <- "BATHURST"
tbl$Station[tbl$Station == "LARAPUNA (EDDYSTONE POINT"] <- "LARAPUNA"
tbl$Station[tbl$Station == "INVERELL (RAGLAN ST"] <- "INVERELL"
tbl$Station[tbl$Station == "KALGOORLIE-BOULDER "] <- "KALGOORLIE"
tbl$Station[tbl$Station == "MELBOURNE (OLYMPIC PARK"] <- "MELBOURNE"
tbl$Station[tbl$Station == "SYDNEY (OBSERVATORY HILL"] <- "SYDNEY"
tbl$Station[tbl$Station == "RUTHERGLEN RESEARCH"] <- "RUTHERGLEN"
tbl$Station[tbl$Station == "HOBART (ELLERSLIE ROAD"] <- "HOBART"
tbl$Station[tbl$Station == "CAMOOWEAL TOWNSHIP"] <- "CAMOOWEAL"
tbl$Station[tbl$Station == "CUNDERDIN AIRFIELD"] <- "CUNDERDIN"
tbl$Station[tbl$Station == "GROVE (RESEARCH STATION"] <- "GROVE"
tbl$Station[tbl$Station == "GILES METEOROLOGICAL"] <- "GILES"

# ----------------------------------
# CREATE YEARLY AVERAGE AGGREGATION
# ----------------------------------

# convert to numeric 
tbl$maximum.temperature..degC. <- as.numeric(tbl$maximum.temperature..degC.)
tbl$minimum.temperature..degC. <- as.numeric(tbl$minimum.temperature..degC.)


# -------------------
# ALL SEASONS 
# -------------------

# Aggregate 
All_Seasons <- tbl %>% 
  group_by(Year, Station, Season) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T,),2))

All_Seasons <- as.data.frame(All_Seasons)
All_Seasons$avg_max <- as.numeric(All_Seasons$avg_max)
All_Seasons$avg_min <- as.numeric(All_Seasons$avg_min)

# max bins
All_Seasons <- All_Seasons %>%
  # convert state to factor and reverse order of levels
  mutate(Station=factor(Station,levels=rev(sort(unique(Station))))) %>%
  # create a new variable from count
  mutate(maxbin=cut(avg_max,
                    breaks=c(0,5,10,15,20,25,30,35,40,max(avg_max,na.rm = T)),
                    labels=c("0-5°", 
                             "5-10°",
                             "10-15°",
                             "15-20°",
                             "20-25°",
                             "25-30°",
                             "30-35°",
                             "35-40°",
                             "40-45°"))) %>%
  # change level order
  mutate(maxbin=factor(as.character(maxbin),levels=rev(levels(maxbin))))

# min bins
All_Seasons <- All_Seasons %>%
  # convert state to factor and reverse order of levels
  mutate(Station=factor(Station,levels=rev(sort(unique(Station))))) %>%
  # create a new variable from count
  mutate(minbin=cut(avg_min,
                    breaks=c(-5,0,5,10,15,20,25,max(avg_min,na.rm = T)),
                    labels=c("-5-0°", 
                             "0-5°", 
                             "5-10°",
                             "10-15°",
                             "15-20°",
                             "20-25°",
                             "25-30°"))) %>%
  # change level order
  mutate(minbin=factor(as.character(minbin),levels=rev(levels(minbin))))

grid.s <- expand.grid(Year = unique(All_Seasons$Year),
                      Station = unique(All_Seasons$Station),
                      Season = unique(All_Seasons$Season))
All_Seasons <- 
  merge(grid.s,
        All_Seasons,
        all = TRUE)

# Maximum Average
Max <- All_Seasons %>% 
  group_by(Station, Season) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
All_Seasons <- left_join(All_Seasons, Max, by = c("Station", "Season")) 

# Maximum Average
Min <- All_Seasons %>% 
  group_by(Station, Season) %>% 
  summarise(Min_Average = round(mean(avg_min, na.rm = TRUE),2)) 
All_Seasons <- left_join(All_Seasons, Min, by = c("Station", "Season")) 

# Convert year from factor back to numeric
All_Seasons$Year <- as.character(All_Seasons$Year)
All_Seasons$Year <- as.numeric(All_Seasons$Year)
# In order to filter range of data
All_Seasons_1960 <- All_Seasons %>% 
  filter(Year >= 1960)
# Convert back to factor 
All_Seasons_1960$Year <- as.factor(All_Seasons_1960$Year)
All_Seasons$Year <- as.factor(All_Seasons$Year)

# Required to custom order facets 
All_Seasons_1960$Season <- factor(All_Seasons_1960$Season, 
                                  levels = c("Winter",
                                             "Autumn",
                                             "Spring",
                                             "Summer"))

# Determine the buckets we want for our legend
# Alternative legend display to the usual gradient 

sl2 <- c(2,5,10,15,20,25,30,35,40,43)
TILE.MAX.SEASONS <- All_Seasons_1960 %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name)) + 
  geom_tile(aes(fill = avg_max), # fill based upon total
            colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  facet_grid(~ Season) +
  coord_equal() + # perfect square 
  
  scale_fill_gradient2(low = "#78e3eb",
                       mid = "#ffe747",
                       high = "#e62a19",
                       midpoint = 22,
                       na.value = "#fffdf2",
                       name = "Average Daily Max",
                       guide = "legend", # can specify colour scale, or legend format
                       breaks = sl2) + # specify breaks (this does not work here)
  
  # Guides - a useful way to format legend 
  guides(fill = guide_legend(title = "°C", # title of legend
                             title.hjust = 0.4, # centre title
                             title.vjust = 0.4, # centre title
                             breaks = sl2, # specify breaks for legend format rather than colour scale
                             reverse = TRUE)) + # descending rather than ascending order for scale
  
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia across the seasons (°C)",
       subtitle = "Temperature recordings from 112 weather stations (y-axis) between 1960 & 2019 (x-axis)",
       caption = "Data Source: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1960","1970","1980","1990","2000","2010")) +

  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 6,
                                   colour = "#4E4F4E"),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, colour = "#6b6e6b"),
        plot.background = element_rect(fill = "#fffdf2"),
        legend.background = element_rect(fill = "#fffdf2"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.SEASONS

ggsave(TILE.MAX.SEASONS,
       filename="All-Seasons-max2.png",
       height = 8.8,
       width = 16.8,
       units = "in",
       dpi = 200)


# ODONATTA IN 1994 APPEARS TO BE AN OUTLIER
Yearly_Average2 <- Yearly_Average %>% 
  mutate(avg_max = na_if(avg_max, 43.80)) %>% 
  filter(Year != 2019)


sl3 <- c(10,15,20,25,30,35,37)
TILE.MAX <- Yearly_Average2 %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name)) + 
  geom_tile(aes(fill = avg_max),
            colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  
  scale_fill_gradient2(low = "#78e3eb",
                       mid = "#ffe747",
                       high = "#e62a19",
                       midpoint = 25,
                       na.value = "#fffdf2",
                       name = "Average Daily Max",
                       guide = "legend", # can specify colour scale, or legend format
                       breaks = sl3) + # specify breaks (this does not work here)
  
  # Guides - a useful way to format legend 
  guides(fill = guide_legend(title = "°C", # title of legend
                             title.hjust = 0.2, # centre title
                             title.vjust = 0.2, # centre title
                             breaks = sl2, # specify breaks for legend format rather than colour scale
                             reverse = TRUE)) + # descending rather than ascending order for scale
  
  
  coord_equal() + # perfect square 
  # guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2018",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  # scale_fill_manual(values=WarmPalette,
  #                   na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 6,
                                   colour = "#4E4F4E"),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        plot.background = element_rect(fill = "#fffdf2"),
        legend.background = element_rect(fill = "#fffdf2"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX



# Save Plot
ggsave(TILE.MAX,
       filename = "avg-max.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)










# ----------------------------------
# CREATE AESTHETICS FOR VISUALISATIONS 
# ----------------------------------

# assign text colour for visualisation 
textcol <- "grey40"
texthot <- "#6b3029"
textcold <- "#3d6c80"

# Warm Colours
WarmPalette <- c("40-45°" = "#780018", 
                 "35-40°" = "#96002b", 
                 "30-35°" = "#d13024", 
                 "25-30°" = "#e87a13", 
                 "20-25°" = "#edb41c", 
                 "15-20°" = "#e8d18e", 
                 "10-15°" = "#f2eaac", 
                 "5-10°" = "#fcf8d4",
                 "0-5°" = "#fff9e6")


# Cool Colours
CoolPalette <- c("25-30°" = "#c9f2ed", 
                 "20-25°" = "#acebf2", 
                 "15-20°" = "#85def2", 
                 "10-15°" = "#5cc1e6", 
                 "5-10°" = "#30a1c7",
                 "0-5°" = "#167dab",
                 "-5-0°" = "#055a87")

# Stylish Colours
# Warm Colours
StatePalette <- c("40-45°" = "#993B37", 
                  "35-40°" = "#cf5f1f", 
                  "30-35°" = "#C88370", 
                  "25-30°" = "#e39724", 
                  "20-25°" = "#edb41c", 
                  "15-20°" = "#e5e84a", 
                  "10-15°" = "#dbe899", 
                  "5-10°" = "#7AA2BE",
                  "0-5°" = "#9bcbcf")