# ----------------------------------
# DEPENDENCIES 
# ----------------------------------

library(tidyverse)
library(forcats)
library(data.table)
library(anytime)
library(lubridate)
library(zoo)
library(plotly)

# ----------------------------------
# DATA-SOURCE
# ----------------------------------

tbl <- read.csv("/Users/perkot/Dropbox/Analytics/Personal/R/git/ACORN-SAT-Climate-Data-gganimate/ACORN-SAT-Clean.csv")

str(tbl)

# ----------------------------------
# AGGREGATE
# ----------------------------------

# Daily avg temp in AU
aus.day.avg <- tbl %>% 
  group_by(date) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm = TRUE),1))

# ensure in df format 
aus.day.avg <- as.data.frame(aus.day.avg)

# ----------------------------------
# DATES
# ----------------------------------

# Extract out day from date for visualisation 
aus.day.avg$day <- substr(x = aus.day.avg$date, 
                               start = 9, 
                               stop = 10)

# Create "Year-Month" column - y axis 
setDT(aus.day.avg)[, yr_month := format(as.Date(date), "%Y-%m") ]

# Create year column - y axis labels 
aus.day.avg$Year <- year(aus.day.avg$date)

# ----------------------------------
# FILTER
# ----------------------------------

aus.day.avg.08.18 <- aus.day.avg %>% 
  filter(Year >= 2008) %>% 
  filter(Year <= 2018)

# ----------------------------------
# FILL-OUT
# ----------------------------------

# Complete out dates
aus.day.avg.08.18 <- 
  aus.day.avg.08.18 %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))

# Replace NA values with a 0, reflecting no listens on that date 
aus.day.avg.08.18[is.na(aus.day.avg.08.18)] <- 0

# ----------------------------------
# VISUALISE 
# ----------------------------------

# Temp Range
  # 15 to 37

# For legend
tr <- c(15,17,19,21,23,25,27,29,31,33,35,37)

# gradient between colour [1] & colour [2]
colfunc <- colorRampPalette(c("#CF3D38","#B68444","#4CA4A5"))
# plot gradient
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)
# generate 11 colours ranging from colour [1] to colour [2]
colfunc(11)



# Create tile-plot
p <- 
  aus.day.avg.08.18 %>% 
  ggplot(aes(x = yr_month,
             forcats::fct_rev(day))) + # reverse order of y-axis
  
  geom_tile(aes(fill = avg_max), # fill based upon total
            colour = "white", # border colour == white
            size = 0.2, # size of tile
            na.rm = FALSE) + # don't remove missing values 
  coord_equal(ratio = 1) +  # keep even squares
  
  scale_fill_gradient2(low = "#78ebe9",
                       mid = "#f5ea78",
                       high = "#e33627",
                       midpoint = 28,
                       na.value = "#f2eba2",
                       name = "Average Daily Max",
                       guide = "legend", # can specify colour scale, or legend format
                       breaks = tr) + # specify breaks (this does not work here)
  
  labs(x = "",
       y = "",
       title = "Average of daily maximum temperature °C between 2008 & 2018",
       subtitle = "Average value calculated from 112 weather stations across AU ",
       caption = "Data-source: ACORN-SAT") +
  
  # Guides - a useful way to format legend 
  guides(fill = guide_legend(title = "Max °C", # title of legend
                             title.hjust = 0.5, # centre title
                             title.vjust = 0.5, # centre title
                             breaks = tr, # specify breaks for legend format rather than colour scale
                             reverse = TRUE)) + # descending rather than ascending order for scale
  
  # Specify custom axis labels 
  # modify to "year" view for y-axis 
  scale_x_discrete(
    breaks = c("2008-01", "2009-01", "2010-01", "2011-01", "2012-01", "2013-01", "2014-01", "2015-01", "2016-01", "2017-01", "2018-01"), # pick only first-month
    labels = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")) + # label it as year - cleaner aesthetic
  # remove leading zero from x-axis 
  scale_y_discrete(
    breaks = c("01", "05", "10", "15", "20", "25", "30"), # pick only first-month
    labels = c("1", "5", "10", "15", "20", "25", "30")) + # label it as year - cleaner aesthetic
  
  # crucial - removes gray areas from empty cells (i.e. February 30)
  theme(panel.background = element_rect(fill = "#FFFDFA")) + 
  
  # Titles / Text - size, colours, vjust 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#1C2226"),
        axis.text.x = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#1C2226"),
        plot.title = element_text(colour = "#3b474f",
                                  hjust = 0,
                                  size = 9,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#5d6b75",
                                     hjust = 0,
                                     size = 8),
        plot.caption = element_text(colour = "#5d6b75",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 7,
                                    face = "italic",
                                    margin = margin(5,0,0,0)), # adjust position ... top, bottom, left, right
        
        # Remove ticks/titles/labels I don't want 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        
        # Aesthetic of legend 
        legend.position = "right", # position to right of plot
        legend.direction = "vertical", # vertical orientation 
        legend.title = element_text(colour = "#1C2226", # colour of title text
                                    size = 8), # size of text 
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        legend.text = element_text(colour = "#1C2226",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.box.just = "center",
        
        # Plot margins / border / fill-colour 
        plot.background = element_rect(fill = "#fafdff"),
        legend.background = element_rect(fill = "#fafdff"),
        panel.border = element_blank(),
        plot.margin = margin(2,1,2,1,"cm") # top, right, bottom, left
  ) 
p












# ----------------------------------
# FILTER
# ----------------------------------

aus.day.avg.13.18 <- aus.day.avg %>% 
  filter(Year >= 2013) %>% 
  filter(Year <= 2018)

# ----------------------------------
# FILL-OUT
# ----------------------------------

# Complete out dates
aus.day.avg.13.18 <- 
  aus.day.avg.13.18 %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))

# Replace NA values with a 0, reflecting no listens on that date 
aus.day.avg.13.18[is.na(aus.day.avg.13.18)] <- 0

# ----------------------------------
# VISUALISE 
# ----------------------------------

# Temp Range
# 15 to 37

# For legend
tr2 <- c(16,18,20,22,24,26,28,30,32,34,36)

# gradient between colour [1] & colour [2]
colfunc <- colorRampPalette(c("#CF3D38","#B68444","#4CA4A5"))
# plot gradient
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)
# generate 11 colours ranging from colour [1] to colour [2]
colfunc(11)



# Create tile-plot

# Colours # 1
  # low = "#78ebe9",
  # mid = "#f5ea78",
  # high = "#e33627",  

# Colours # 2
  # low = "#3C86A3",
  # mid = "#D9CAB0",
  # high = "#8C2626",  

p <- 
  aus.day.avg.13.18 %>% 
  ggplot(aes(x = yr_month,
             forcats::fct_rev(day))) + # reverse order of y-axis
  
  geom_tile(aes(fill = avg_max), # fill based upon total
            colour = "white", # border colour == white
            size = 0.2, # size of tile
            na.rm = FALSE) + # don't remove missing values 
  coord_equal(ratio = 1) +  # keep even squares
  
  scale_fill_gradient2(low = "#8ce36d",
                       mid = "#fceb68",
                       high = "#eb0c26",
                       midpoint = 28,
                       na.value = "#fcf8ed",
                       name = "Average Daily Max",
                       guide = "legend", # can specify colour scale, or legend format
                       breaks = tr2) + # specify breaks (this does not work here)
  
  labs(x = "",
       y = "",
       title = "Daily maximum temperature °C across Australia between 2013 & 2018",
       subtitle = "Average °C derived from 112 weather stations across AU ",
       caption = "Data-source: ACORN-SAT") +
  
  # Guides - a useful way to format legend 
  guides(fill = guide_legend(title = "Max °C", # title of legend
                             title.hjust = 0.5, # centre title
                             title.vjust = 0.5, # centre title
                             breaks = tr, # specify breaks for legend format rather than colour scale
                             reverse = TRUE)) + # descending rather than ascending order for scale
  
  # Specify custom axis labels 
  # modify to "year" view for y-axis 
  scale_x_discrete(
    breaks = c("2013-01", "2014-01", "2015-01", "2016-01", "2017-01", "2018-01"), # pick only first-month
    labels = c("2013", "2014", "2015", "2016", "2017", "2018")) + # label it as year - cleaner aesthetic
  # remove leading zero from x-axis 
  scale_y_discrete(
    breaks = c("01","05","10","15","20","25","31"), # pick only first-month
    labels = c("1","5","10","15","20","25","31")) + # label it as year - cleaner aesthetic
  
  # crucial - removes gray areas from empty cells (i.e. February 30)
  theme(panel.background = element_rect(fill = "#FFFDFA")) + 
  
  # Titles / Text - size, colours, vjust 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#1C2226"),
        axis.text.x = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#1C2226"),
        plot.title = element_text(colour = "#3b474f",
                                  hjust = 0,
                                  size = 9,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#5d6b75",
                                     hjust = 0,
                                     size = 8,
                                     margin = margin(0,3,1,0)),
        plot.caption = element_text(colour = "#5d6b75",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 7,
                                    face = "italic",
                                    margin = margin(5,0,0,0)), # adjust position ... top, bottom, left, right
        
        # Remove ticks/titles/labels I don't want 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        
        # Aesthetic of legend 
        legend.position = "right", # position to right of plot
        legend.direction = "vertical", # vertical orientation 
        legend.title = element_text(colour = "#1C2226", # colour of title text
                                    size = 8), # size of text 
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        legend.text = element_text(colour = "#1C2226",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.box.just = "center",
        
        # Plot margins / border / fill-colour 
        plot.background = element_rect(fill = "#fffbeb"),
        legend.background = element_rect(fill = "#fffbeb"),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#fffbeb", colour = "#fffeeb"),
        plot.margin = margin(2,1,2,1,"cm") # top, right, bottom, left
  ) 
p

# Save Plot
ggsave(p,
       filename = "avg-max-day-aus.png",
       height = 8.8,
       width = 8.8,
       units = "in",
       dpi = 200)


