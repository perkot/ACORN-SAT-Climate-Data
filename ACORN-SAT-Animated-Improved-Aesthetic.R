# Dependencies 
require(tidyverse)
require(readr)
require(ggplot2)
require(gganimate)
require(viridis)
require(scales)
require(gifski)
require(png)
require(transformr)
require(kableExtra)

# read in data
tbl <- read.csv("ACORN-SAT-Clean.csv")

# color themes 
state.colors.gradient.2 <- 
  c("NT" = "#993B37", 
    "TAS" = "#7AA2BE", 
    "VIC" = "#4EA599",  
    "SA" = "#E1A345", 
    "NSW" = "#D2AF47", 
    "WA" = "#C88370", 
    "QLD" = "#CA6A33") 

state.colors.gradient.3 <- 
  c("NT" = "#993B37", 
    "TAS" = "#7AA2BE", 
    "VIC" = "#4ea58e",  
    "SA" = "#e3963d", 
    "NSW" = "#D2AF47", 
    "WA" = "#d97529", 
    "QLD" = "#c95042") 

# Themes for plot visualisations 
theme_plot <-
  theme(
    plot.title = element_text(size = 14, hjust = 0, colour = "#4E4F4E", face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0, colour = "#4E4F4E"),
    axis.title = element_text(size = 12, colour = "#4E4F4E"),
    legend.title = element_text(size = 12, colour = "#4E4F4E"),
    axis.text = element_text(size = 12, colour = "#4E4F4E"),
    panel.background = element_rect(fill = "#fcf9f0",
                                    colour = "#fcf9f0"),
    plot.background = element_rect(fill = "#fcf9f0",
                                   colour = "#fcf9f0"))

theme_plot_2 <-
  theme(
    plot.title = element_text(size = 12, hjust = 0, colour = "#4E4F4E", face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0, colour = "#4E4F4E"),
    axis.title = element_text(size = 9, colour = "#4E4F4E"),
    legend.title = element_text(size = 10, hjust = 0.5, colour = "#4E4F4E"),
    axis.text = element_text(size = 9, colour = "#4E4F4E"),
    panel.background = element_rect(fill = "#fcf9f0",
                                    colour = "#fcf9f0"),
    plot.background = element_rect(fill = "#fcf9f0",
                                   colour = "#fcf9f0"))

# ---------------
# ANIMATED ALL SEASONS
# ---------------

# Create table
temp.max3C <- tbl %>% 
  group_by(State, Era) %>% 
  filter(Era != 2019) %>% 
  summarise(avgmax = mean(maximum.temperature..degC., na.rm = TRUE))

# as character
temp.max3C$Era <- as.character(temp.max3C$Era)
# remove s
temp.max3C$Era = substr(temp.max3C$Era,1,nchar(temp.max3C$Era)-1)
# to numeric
temp.max3C$Era <- as.numeric(temp.max3C$Era)

# PLOT
state.plot.2 <- 
  ggplot(temp.max3C, aes(Era, avgmax, group = State, color = State)) + 
  geom_line(size = 1.6) + 
  geom_segment(aes(xend = 2018, yend = avgmax), linetype = 2, colour = 'grey') + 
  geom_point(size = 8) + 
  geom_text(aes(x = 2018, label = State, size = 16), hjust = 0) + 
  scale_colour_manual(values = state.colors.gradient.2) +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Average daily-maximum temperature in Australia over the past 100 years',
       subtitle = 'Separated by State',
       caption  = "Data Source: ACORN-SAT",
       y = 'Temperature (°C)',
       X = "Year") +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) + 
  theme_plot +
  transition_reveal(Era) 

# ANIMATE PLOT
state.plot.2.anim <-
animate(state.plot.2,
        end_pause = 80,
        fps = 30,
        nframe = 240,
        height = 1024,
        width = 768)

state.plot.2.anim

# SAVE PLOT 
anim_save("state.plot.2.anim.gif",
          state.plot.2.anim,
          width = 4000, 
          height = 4000)


# ---------------
# ANIMATED SUMMER
# ---------------

# Create table
temp.max3C.sum <- tbl %>% 
  filter(Season == "Summer") %>% 
  group_by(State, Era) %>% 
  filter(Era != 2019) %>% 
  summarise(avgmax = mean(maximum.temperature..degC., na.rm = TRUE))

# as character
temp.max3C.sum$Era <- as.character(temp.max3C.sum$Era)
# remove s
temp.max3C.sum$Era = substr(temp.max3C.sum$Era,1,nchar(temp.max3C.sum$Era)-1)
# to numeric
temp.max3C.sum$Era <- as.numeric(temp.max3C.sum$Era)

# PLOT 
state.plot.2.sum <- 
  ggplot(temp.max3C.sum, aes(Era, avgmax, group = State, color = State)) + 
  geom_line(size = 1.6) + 
  geom_segment(aes(xend = 2018, yend = avgmax), linetype = 2, colour = 'grey') + 
  geom_point(size = 8) + 
  geom_text(aes(x = 2018, label = State, size = 16), hjust = 0) + 
  scale_colour_manual(values = state.colors.gradient.2) +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Average daily-maximum temperature in Australia over the past 100 years',
       subtitle = 'Separated by State',
       caption  = "Data Source: ACORN-SAT",
       y = 'Temperature (°C)',
       X = "Year") +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) + 
  theme_plot +
  transition_reveal(Era) 

# ANIMATE PLOT
state.plot.2.sum.anim <-
  animate(state.plot.2.sum,
          end_pause = 80,
          fps = 30,
          nframe = 240,
          height = 1024,
          width = 768)

state.plot.2.sum.anim

# SAVE PLOT
anim_save("state.plot.2.anim.gif",
          state.plot.2.anim,
          width = 4000, 
          height = 4000)

# ---------------
# STATE & WEATHER STATION 
# ---------------

# try to re-create
# https://www.vizwiz.com/2017/11/life-expectancy.html

# Create table
test <- tbl %>% 
  group_by(site.name, State, Era) %>% 
  filter(Era != 2019) %>% 
  summarise(avgmax = mean(maximum.temperature..degC., na.rm = TRUE))

test2 <- tbl %>% 
  group_by(State, Era) %>% 
  filter(Era != 2019) %>% 
  summarise(avgmax = mean(maximum.temperature..degC., na.rm = TRUE))

# Change smooth defaults
update_geom_defaults("smooth", list(size = .5))

# convert state levels to factor to customise order
test$State <- factor(test$State, 
                     levels=c("NT", "QLD", "WA", "SA", "NSW", "VIC", "TAS"), 
                     labels=c("NT", "QLD", "WA", "SA", "NSW", "VIC", "TAS"))


# CREATE PLOT 
ss <- 
ggplot(test, 
       aes(x = Era, 
          y = avgmax, 
          group = site.name, 
          color = State)) + 
  # geom_point(size = 2,
  #            position = position_jitterdodge(jitter.width = 0.5,
  #                                            jitter.height = 0.2,
  #                                            dodge.width = 0.2)) + 
  
  geom_point(size = 6,
             shape = 1,
             stroke = 1) + 
  
  # geom_line(data = test2, aes(stat = "smooth",
  #                             alpha = 0.8),
  #           show.legend = FALSE,
  #           size = 0.1) +
  # geom_line(data = test2) + 
  
  geom_line(aes(stat = "smooth", alpha = 0.5),
            show.legend = FALSE,
            size = 0.15) +
  
  # geom_jitter(width = 0.2, 
  #             height = 0.2,
  #             alpha = I(1 / 2),
  #             na.rm = TRUE
  #             ) +
  
  scale_y_continuous(breaks = seq(10, 40, 5),
                     labels = paste0(seq(10, 40, 5),"°")) + # paste0 to add label to axis
  scale_colour_manual(values = state.colors.gradient.3) +
  labs(title = 'Average daily-maximum temperature in Australia per decade',
       subtitle = 'Separated by State & Weather Station',
       caption  = "Data Source: The Australian Climate Observations Reference Network – Surface Air Temperature (ACORN-SAT)") +
  theme_minimal() + 
  theme(legend.position = "right") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, 
                                    face = "italic", 
                                    size = 6,
                                    colour = "#4E4F4E")) +
  theme(plot.margin = margin(1.5,1.5,1.5,1.5,"cm")) +
  theme_plot_2 

# PRINT PLOT
ss

# SAVE PLOT
ggsave(ss,
       filename="All-states-max.png",
       height = 8.8,
       width = 14.8,
       units = "in",
       dpi = 200)






# --------

# Create table
temp.max.site.name <- tbl %>% 
  group_by(site.name, Era) %>% 
  filter(Era != 2019) %>% 
  filter(Season == "Summer") %>% 
  summarise(avgmax = mean(maximum.temperature..degC., na.rm = TRUE))

change <- temp.max.site.name %>% 
  filter(Era == "1910s" | Era == "2010s")

# Long to wide to only include complete cases
change2 <- change %>%
  tidyr::spread(key = Era, value = avgmax) %>% 
  na.omit() 

change2 <- change2 %>%
  mutate(Difference = `2010s` - `1910s`) %>%
  mutate(Percent_change = (Difference / `1910s`)*100)

gg <- 
  ggplot(change2, aes(x=`1910s`, 
                      xend=`2010s`, 
                      y=reorder(site.name, -`2010s`), 
                      group=site.name)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75,
                size_x=2,	
                size_xend=2,
                point.colour.l="#0e668b") + 
  # scale_x_continuous(label=Difference) + 
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart", 
       subtitle="°C Change: 1910s to 2010s", 
       caption="Data Source: ACORN-SAT") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)



