library(ggalluvial)

detach(package:plyr)

library(dplyr)

tbl.agg <- tbl %>% 
  group_by(Season, State) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T),2)) 

library(data.table)
long <- melt(setDT(tbl.agg), 
             id.vars = c("Season","State"), 
             variable.name = "Avg_Temp")


is_alluvia_form(as.data.frame(tbl.agg), axes = 1:2, silent = TRUE)


ggplot(as.data.frame(long),
       aes(y = value, axis1 = Season, axis2 = State)) +
  geom_alluvium(aes(fill = Avg_Temp), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")

tbl$location2 <- as.character(tbl$location2)

Daily_Average <- tbl %>% 
  # select(site.name, date, maximum.temperature..degC., Season, State)
  group_by(date) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2)) 

# Standard .csv export 
write.csv(Daily_Average, file = "Daily_Average.csv",
          na = "", 
          row.names = FALSE)

# ----------------------------------
# CREATE YEARLY AVERAGE AGGREGATION
# ----------------------------------

# Aggregate 
Yearly_Average <- tbl %>% 
  group_by(Year, site.name) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2),
            avg_min = round(mean(minimum.temperature..degC., na.rm=T),2)) 

# Standard .csv export 
write.csv(Yearly_Average, file = "Yearly_Average.csv",
          na = "", 
          row.names = FALSE)

# ----------------------------------
# DATA CLEAN 
# ----------------------------------

# Abbrevate Station names 

# Sub-out only first word 
# Yearly_Average2$Station <- gsub("([A-Za-z]+).*", "\\1", 
#                                 Yearly_Average2$site.name)

# Sub out only second word 
Yearly_Average$Station <- gsub("(\\w+\\s+\\w+).*", "\\1", 
                               Yearly_Average$site.name)

# Get unique station names 
unique_station <- unique(Yearly_Average$Station)
unique_station

# Remove these words 
Yearly_Average <- Yearly_Average %>% mutate_all(~gsub("AIRPORT", "", .))
Yearly_Average <- Yearly_Average %>% mutate_all(~gsub("AERO", "", .))
Yearly_Average <- Yearly_Average %>% mutate_all(~gsub("RAAF", "", .))

# Change these station names 
Yearly_Average$Station[Yearly_Average$Station == "BATHURST AGRICULTURAL"] <- "BATHURST"
Yearly_Average$Station[Yearly_Average$Station == "LARAPUNA (EDDYSTONE POINT"] <- "LARAPUNA"
Yearly_Average$Station[Yearly_Average$Station == "INVERELL (RAGLAN ST"] <- "INVERELL"
Yearly_Average$Station[Yearly_Average$Station == "KALGOORLIE-BOULDER "] <- "KALGOORLIE"
Yearly_Average$Station[Yearly_Average$Station == "MELBOURNE (OLYMPIC PARK"] <- "MELBOURNE"
Yearly_Average$Station[Yearly_Average$Station == "SYDNEY (OBSERVATORY HILL"] <- "SYDNEY"
Yearly_Average$Station[Yearly_Average$Station == "RUTHERGLEN RESEARCH"] <- "RUTHERGLEN"
Yearly_Average$Station[Yearly_Average$Station == "HOBART (ELLERSLIE ROAD"] <- "HOBART"
Yearly_Average$Station[Yearly_Average$Station == "CAMOOWEAL TOWNSHIP"] <- "CAMOOWEAL"
Yearly_Average$Station[Yearly_Average$Station == "CUNDERDIN AIRFIELD"] <- "CUNDERDIN"
Yearly_Average$Station[Yearly_Average$Station == "GROVE (RESEARCH STATION"] <- "GROVE"
Yearly_Average$Station[Yearly_Average$Station == "GILES METEOROLOGICAL"] <- "GILES"

# Ensure dataframe
Yearly_Average <- as.data.frame(Yearly_Average)

# Convert 'year' to factor
Yearly_Average$Year <- as.factor(Yearly_Average$Year)

# Convert average temperatures to numeric 
Yearly_Average$avg_max <- as.numeric(Yearly_Average$avg_max)
Yearly_Average$avg_min <- as.numeric(Yearly_Average$avg_min)

# ----------------------------------
# DATA VISUALISATION  
# ----------------------------------

# https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/

# assign text colour for visualisation 
textcol <- "grey40"
texthot <- "#6b3029"
textcold <- "#3d6c80"

# Create buckets for avg_max
Yearly_Average2 <- Yearly_Average %>%
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

# Create buckets for avg_min
Yearly_Average2 <- Yearly_Average2 %>%
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

# Where stations do not have temperature data, create empty records 

# https://stackoverflow.com/questions/9996452/r-find-and-add-missing-non-existing-rows-in-time-related-data-frame
vals <- expand.grid(Year = unique(Yearly_Average2$Year),
                    Station = unique(Yearly_Average2$Station))
Yearly_Average3 <- 
  merge(vals,
        Yearly_Average2,
        all = TRUE)

# Maximum Average
Max <- Yearly_Average3 %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Yearly_Average3 <- left_join(Yearly_Average3, Max, by = "Station") 

# Minimum Average
Min <- Yearly_Average3 %>% 
  group_by(Station) %>% 
  summarise(Min_Average = round(mean(avg_min, na.rm = TRUE),2)) 
Yearly_Average3 <- left_join(Yearly_Average3, Min, by = "Station") 



WarmPalette <- c("40-45°" = "#780018", 
                 "35-40°" = "#96002b", 
                 "30-35°" = "#d13024", 
                 "25-30°" = "#e87a13", 
                 "20-25°" = "#edb41c", 
                 "15-20°" = "#e8d18e", 
                 "10-15°" = "#f2eaac", 
                 "5-10°" = "#fcf8d4",
                 "0-5°" = "#fff9e6")

  
CoolPalette <- c("25-30°" = "#d5f2ee", 
                "20-25°" = "#b6ecf2", 
                "15-20°" = "#8ddef0", 
                "10-15°" = "#5cc1e6", 
                "5-10°" = "#30a1c7",
                "0-5°" = "#167dab")



library(ggplot2)
library(forcats)

# further modified ggplot
p <- Yearly_Average3 %>% 
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
p

ggsave(p,filename="avg-max.png",height=8.8,width=8.8,units="in",dpi=200)


# further modified ggplot
p2 <- Yearly_Average3 %>% 
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
p2

ggsave(p2,filename="avg-min.png",height=8.8,width=8.8,units="in",dpi=200)












# Aggregate 
Summer_Max <- tbl %>% 
  filter(Season == "Summer") %>% 
  group_by(Year, site.name) %>% 
  summarise(avg_max = round(mean(maximum.temperature..degC., na.rm=T,),2)) 


# Abbrevate Station names 

# Sub-out only first word 
# Yearly_Average2$Station <- gsub("([A-Za-z]+).*", "\\1", 
#                                 Summer_Max2$site.name)

# Sub out only second word 
Summer_Max$Station <- gsub("(\\w+\\s+\\w+).*", "\\1", 
                           Summer_Max$site.name)

# Remove these words 
Summer_Max <- Summer_Max %>% mutate_all(~gsub("AIRPORT", "", .))
Summer_Max <- Summer_Max %>% mutate_all(~gsub("AERO", "", .))
Summer_Max <- Summer_Max %>% mutate_all(~gsub("RAAF", "", .))

# Change these station names 
Summer_Max$Station[Summer_Max$Station == "BATHURST AGRICULTURAL"] <- "BATHURST"
Summer_Max$Station[Summer_Max$Station == "LARAPUNA (EDDYSTONE POINT"] <- "LARAPUNA"
Summer_Max$Station[Summer_Max$Station == "INVERELL (RAGLAN ST"] <- "INVERELL"
Summer_Max$Station[Summer_Max$Station == "KALGOORLIE-BOULDER "] <- "KALGOORLIE"
Summer_Max$Station[Summer_Max$Station == "MELBOURNE (OLYMPIC PARK"] <- "MELBOURNE"
Summer_Max$Station[Summer_Max$Station == "SYDNEY (OBSERVATORY HILL"] <- "SYDNEY"
Summer_Max$Station[Summer_Max$Station == "RUTHERGLEN RESEARCH"] <- "RUTHERGLEN"
Summer_Max$Station[Summer_Max$Station == "HOBART (ELLERSLIE ROAD"] <- "HOBART"
Summer_Max$Station[Summer_Max$Station == "CAMOOWEAL TOWNSHIP"] <- "CAMOOWEAL"
Summer_Max$Station[Summer_Max$Station == "CUNDERDIN AIRFIELD"] <- "CUNDERDIN"
Summer_Max$Station[Summer_Max$Station == "GROVE (RESEARCH STATION"] <- "GROVE"
Summer_Max$Station[Summer_Max$Station == "GILES METEOROLOGICAL"] <- "GILES"

# Ensure dataframe
Summer_Max <- as.data.frame(Summer_Max)

# Convert 'year' to factor
Summer_Max$Year <- as.factor(Summer_Max$Year)

# Convert average temperatures to numeric 
Summer_Max$avg_max <- as.numeric(Summer_Max$avg_max)

# ----------------------------------
# DATA VISUALISATION  
# ----------------------------------

# Create buckets for avg_max
Summer_Max2 <- Summer_Max %>%
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

# https://stackoverflow.com/questions/9996452/r-find-and-add-missing-non-existing-rows-in-time-related-data-frame
vals <- expand.grid(Year = unique(Summer_Max2$Year),
                    Station = unique(Summer_Max2$Station))
Summer_Max3 <- 
  merge(vals,
        Summer_Max2,
        all = TRUE)

# Maximum Average
Max <- Summer_Max3 %>% 
  group_by(Station) %>% 
  summarise(Max_Average = round(mean(avg_max, na.rm = TRUE),2)) 
Summer_Max3 <- left_join(Summer_Max3, Max, by = "Station") 

library(ggplot2)
library(forcats)

# further modified ggplot
s <- Summer_Max3 %>% 
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
s

ggsave(s,filename="summer-max.png",height=8.8,width=8.8,units="in",dpi=200)

