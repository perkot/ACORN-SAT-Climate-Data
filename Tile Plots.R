# ----------------------------------
# DEPENDENCIES 
# ----------------------------------

library(tidyverse)
library(forcats)

# ----------------------------------
# DATA-SOURCE
# ----------------------------------

tbl <- read.csv("ACORN-SAT-Clean.csv")

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

# Aggregate 
Yearly_Average <- tbl %>% 
  group_by(Year, Station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T),2)) 

# Ensure dataframe
Yearly_Average <- as.data.frame(Yearly_Average)

# Convert 'year' to factor
Yearly_Average$Year <- as.factor(Yearly_Average$Year)

# Convert average temperatures to numeric 
Yearly_Average$avg_max <- as.numeric(Yearly_Average$avg_max)
Yearly_Average$avg_min <- as.numeric(Yearly_Average$avg_min)

# ----------------------------------
# PREPARE FOR DATA VISUALISATION  
# ----------------------------------

# https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/

# -------------------
# BUCKET MAX TEMPS 
# -------------------

# Create buckets for avg_max
Yearly_Average <- Yearly_Average %>%
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

# -------------------
# BUCKET MIN TEMPS 
# -------------------

# Create buckets for avg_min
Yearly_Average <- Yearly_Average %>%
  # convert state to factor and reverse order of levels
  mutate(Station = factor(Station,
                          levels = rev(sort(unique(Station))))) %>%
  # create a new variable from count
  mutate(minbin=cut(avg_min,
                    breaks=c(0,5,10,15,20,25,30,35,40,max(avg_min,na.rm = T)),
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
  mutate(minbin=factor(as.character(minbin),levels=rev(levels(minbin))))

# -------------------
# CREATE "DATA GRID"
# -------------------

# Where stations do not have temperature data, create empty records 
# https://stackoverflow.com/questions/9996452/r-find-and-add-missing-non-existing-rows-in-time-related-data-frame

grid <- expand.grid(Year = unique(Yearly_Average$Year),
                    Station = unique(Yearly_Average$Station))
Yearly_Average <- 
  merge(grid,
        Yearly_Average,
        all = TRUE)

# -------------------
# GET AVERAGE TEMP PER STATION FOR ORDERING 
# -------------------

# Maximum Average
Max <- Yearly_Average %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Yearly_Average <- left_join(Yearly_Average, Max, by = "Station") 

# Minimum Average
Min <- Yearly_Average %>% 
  group_by(Station) %>% 
  summarise(Min_Average = round(mean(avg_min, na.rm = TRUE),2)) 
Yearly_Average <- left_join(Yearly_Average, Min, by = "Station") 

rm(Max, Min)

# ----------------------------------
# KEEP A .CSV
# ----------------------------------

# Standard .csv export 
write.csv(Yearly_Average, file = "Yearly_Average.csv",
          na = "", 
          row.names = FALSE)

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
CoolPalette <- c("25-30°" = "#d5f2ee", 
                 "20-25°" = "#b6ecf2", 
                 "15-20°" = "#8ddef0", 
                 "10-15°" = "#5cc1e6", 
                 "5-10°" = "#30a1c7",
                 "0-5°" = "#167dab")

# ----------------------------------
# VISUALISE MAX & MIN TEMPS  
# ----------------------------------

# Attempt to build a tool-tip
# new column: text for tooltip:
Yearly_Average <- Yearly_Average %>%
  mutate(text = paste0("average max °C: ", avg_max, 
                       "\n", 
                       "Year: ", Year, 
                       "\n", 
                       "Station: ", Station))

# MAX PLOT

TILE.MAX <- Yearly_Average %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_text(size = 8,
                                   colour = texthot),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = texthot),
        axis.ticks = element_line(size = 0.2),
        # plot.background = element_blank(),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX

ggplotly(TILE.MAX, 
         tooltip = "text")


# Save Plot
ggsave(TILE.MAX,
       filename = "avg-max.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)


# MIN PLOT

TILE.MIN <- Yearly_Average %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, -Min_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = minbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  coord_equal() +
  guides(fill = guide_legend(title = "°C")) +
  labs(x="",
       y="",
       title = "Average yearly minimum temperature in Australia (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=CoolPalette, 
                    na.value = "#e6f7ff")+ 
  #coord_fixed()+
  theme_grey(base_size = 10) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = textcold,size = 10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = textcold, size = 8,face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_text(size = 8,colour = textcold),
        axis.text.y = element_text(size = 4,vjust = 0.2,colour = textcold),
        axis.ticks = element_line(size = 0.2),
        # plot.background = element_blank(),
        plot.background = element_rect(fill = "#f0fcfc"),
        legend.background = element_rect(fill = "#f0fcfc"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = textcold, 
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = textcold,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = textcold,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MIN

ggsave(TILE.MIN,
       filename = "avg-min.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)

# ----------------------------------
# VISUALISE SEASONALLY  
# ----------------------------------

# -------------------
# SUMMER
# -------------------

# Aggregate 
Summer_Max <- tbl %>% 
  filter(Season == "Summer") %>% 
  group_by(Year, Station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2)) 

# Ensure dataframe
Summer_Max <- as.data.frame(Summer_Max)

# Convert 'year' to factor
Summer_Max$Year <- as.factor(Summer_Max$Year)

# Convert average temperatures to numeric 
Summer_Max$avg_max <- as.numeric(Summer_Max$avg_max)

# Create buckets for avg_max
Summer_Max <- Summer_Max %>%
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

# Where stations do not have temperature data, create empty records 
Summer_Max <- 
  merge(grid,
        Summer_Max,
        all = TRUE)

# Maximum Average
Max <- Summer_Max %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Summer_Max <- left_join(Summer_Max, Max, by = "Station") 

# Summer plot
TILE.MAX.SUM <- Summer_Max %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia during Summer (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_text(size = 8,
                                   colour = texthot),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = texthot),
        axis.ticks = element_line(size = 0.2),
        # plot.background = element_blank(),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.SUM

ggsave(TILE.MAX.SUM,
       filename="summer-max.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)

# -------------------
# WINTER
# -------------------

# Aggregate 
Winter_Max <- tbl %>% 
  filter(Season == "Winter") %>% 
  group_by(Year, Station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2)) 

# Ensure dataframe
Winter_Max <- as.data.frame(Winter_Max)

# Convert 'year' to factor
Winter_Max$Year <- as.factor(Winter_Max$Year)

# Convert average temperatures to numeric 
Winter_Max$avg_max <- as.numeric(Winter_Max$avg_max)

# Create buckets for avg_max
Winter_Max <- Winter_Max %>%
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

# Where stations do not have temperature data, create empty records 
Winter_Max <- 
  merge(grid,
        Winter_Max,
        all = TRUE)

# Maximum Average
Max <- Winter_Max %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Winter_Max <- left_join(Winter_Max, Max, by = "Station") 

# Winter plot

TILE.MAX.WIN <- Winter_Max %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia during Winter (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_text(size = 8,
                                   colour = texthot),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = texthot),
        axis.ticks = element_line(size = 0.2),
        # plot.background = element_blank(),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.WIN

ggsave(TILE.MAX.WIN,
       filename="Winter-max.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)

# -------------------
# AUTUMN
# -------------------

# Aggregate 
Autumn_Max <- tbl %>% 
  filter(Season == "Autumn") %>% 
  group_by(Year, Station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2)) 

# Ensure dataframe
Autumn_Max <- as.data.frame(Autumn_Max)

# Convert 'year' to factor
Autumn_Max$Year <- as.factor(Autumn_Max$Year)

# Convert average temperatures to numeric 
Autumn_Max$avg_max <- as.numeric(Autumn_Max$avg_max)

# Create buckets for avg_max
Autumn_Max <- Autumn_Max %>%
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

# Where stations do not have temperature data, create empty records 
Autumn_Max <- 
  merge(grid,
        Autumn_Max,
        all = TRUE)

# Maximum Average
Max <- Autumn_Max %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Autumn_Max <- left_join(Autumn_Max, Max, by = "Station") 

# Autumn plot

TILE.MAX.AUT <- Autumn_Max %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia during Autumn (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_text(size = 8,
                                   colour = texthot),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = texthot),
        axis.ticks = element_line(size = 0.2),
        # plot.background = element_blank(),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.AUT

ggsave(TILE.MAX.AUT,
       filename="Autumn-max.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)


# -------------------
# SPRING
# -------------------

# Aggregate 
Spring_Max <- tbl %>% 
  filter(Season == "Spring") %>% 
  group_by(Year, Station) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2)) 

# Ensure dataframe
Spring_Max <- as.data.frame(Spring_Max)

# Convert 'year' to factor
Spring_Max$Year <- as.factor(Spring_Max$Year)

# Convert average temperatures to numeric 
Spring_Max$avg_max <- as.numeric(Spring_Max$avg_max)

# Create buckets for avg_max
Spring_Max <- Spring_Max %>%
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

# Where stations do not have temperature data, create empty records 
Spring_Max <- 
  merge(grid,
        Spring_Max,
        all = TRUE)

# Maximum Average
Max <- Spring_Max %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Spring_Max <- left_join(Spring_Max, Max, by = "Station") 

# Spring plot

TILE.MAX.SPR <- Spring_Max %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia during Spring (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_text(size = 8,
                                   colour = texthot),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = texthot),
        axis.ticks = element_line(size = 0.2),
        # plot.background = element_blank(),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.SPR

ggsave(TILE.MAX.SPR,
       filename="Spring-max.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)


# -------------------
# ALL SEASONS 
# -------------------

# Aggregate 
All_Seasons <- tbl %>% 
  group_by(Year, Station, Season) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2))

All_Seasons <- as.data.frame(All_Seasons)
All_Seasons$avg_max <- as.numeric(All_Seasons$avg_max)

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

# Attempt to build a tool-tip
# new column: text for tooltip:
All_Seasons_1960 <- All_Seasons_1960 %>%
  mutate(text = paste0(" average max °C: ", avg_max, 
                       "\n", 
                       "Year: ", Year, 
                       "\n", 
                       "Station: ",Station))

TILE.MAX.SEASONS <- All_Seasons_1960 %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  facet_grid(~ Season) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia across the seasons (°C)",
       subtitle = "Temperature recordings from 112 weather stations (y-axis) between 1960 & 2019 (x-axis)",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 4,
                                   colour = texthot),
        axis.text.y = element_text(size = 4,
                                   vjust = 0.2,
                                   colour = texthot),
        axis.ticks = element_line(size = 0.2),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 9, colour = texthot),
        strip.background = element_rect(fill="#fff9f5"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.SEASONS

ggsave(TILE.MAX.SEASONS,
       filename="All-Seasons-max.png",
       height = 8.8,
       width = 16.8,
       units = "in",
       dpi = 200)

library(plotly)

ggplotly(TILE.MAX.SEASONS, tooltip="text")








TILE.MAX.SEASONS_ALL <- All_Seasons %>% 
  # order by station, & average max temperature 
  mutate(name = fct_reorder(Station, Max_Average)) %>%
  ggplot(aes(x = Year,
             y = name,
             fill = maxbin)) + 
  geom_tile(colour = "white",
            size = 0.2,
            na.rm = FALSE) +
  facet_grid(~ Season) +
  coord_equal() + # perfect square 
  guides(fill = guide_legend(title = "°C")) +
  labs(x = "",
       y = "",
       title = "Average yearly maximum temperature in Australia during Spring (°C)",
       subtitle = "Temperature recordings from 112 weather stations between 1910 & 2019",
       caption = "Dataset: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)"
  ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0),
                   breaks = c("1910","1920","1930","1940","1950",
                              "1960","1970","1980","1990","2000","2010")) +
  scale_fill_manual(values=WarmPalette,
                    na.value = "#fcf5e1")+ 
  #coord_fixed()+
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = texthot,
                                    size=10),
        legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = texthot,
                                   size = 8,
                                   face = "bold"),
        legend.key.height = grid::unit(0.8,"cm"),
        legend.key.width = grid::unit(0.2,"cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # axis.text.x = element_text(size = 4,
        #                            colour = texthot),
        # axis.text.y = element_text(size = 4,
        #                            vjust = 0.2,
        #                            colour = texthot),
        # axis.ticks = element_line(size = 0.2),
        plot.background = element_rect(fill = "#fff9f5"),
        legend.background = element_rect(fill = "#fff9f5"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 9, colour = texthot),
        strip.background = element_rect(fill="#fff9f5"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = texthot,
                                  hjust = 0,
                                  size = 11,
                                  face = "bold"),
        plot.subtitle = element_text(colour = texthot,
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = texthot,
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right
TILE.MAX.SEASONS_ALL













