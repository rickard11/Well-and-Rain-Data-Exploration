#Clean and automate hydrographs
library(tidyverse)
library(dplyr)
library(ggplot2)

#Read in allrain data
folder_path <- "Data/Rain/2-10-2026/"
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
get_rain_name <- function(filename) {
  name <- basename(filename)
  name <- str_remove(name, " Rain.*")
  return(name)}
rain_data <- files %>%
  set_names(map_chr(., get_rain_name)) %>%
  map(read_csv)

rain_data <- map_dfr(files, ~ read_csv(.x) %>%
                       mutate(Name = get_rain_name(.x)))
rain_data$Date<-as.Date(rain_data$Date,format="%Y-%m-%d (%a)")

#Define Water Year
rain_data <- rain_data %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))
#calculate yearly statistics

#add lat long
unique(rain_data$Name)
loc<-read.csv("Data/JLDP_well_location.csv")

Rain_map<-left_join(rain_data,loc,by="Name")
Rain_map_2026_storm<-Rain_map[Rain_map$Date>="2025-12-20" & Rain_map$Date<="2026-01-10",]
Rain_map_2026_summary<- Rain_map_2026_storm %>% 
  group_by(Name) %>% 
  summarise(rain= sum(`Rain (in)`)) %>% 
  ungroup()

Rain_map_2026_summary<-Rain_map_2026_summary[Rain_map_2026_summary$Name!="JLDP Gaspar 1" &
                                               Rain_map_2026_summary$Name!="JLDP Oaks 5" &
                                               Rain_map_2026_summary$Name!="JLDP Tinta 1"&
                                               Rain_map_2026_summary$Name!="JLDP Wood Canyon",]
Rain_map_2026_summary<-as.data.frame(Rain_map_2026_summary)
Rain_map_2026_summary<-merge(Rain_map_2026_summary,loc,by="Name")


#map
library(sf)
library(ggplot2)
library(viridis)

ca_map <- map_data("state", region = "california")

x_range <- range(Rain_map_2026_summary$x)
y_range <- range(Rain_map_2026_summary$y)

ggplot() +
  geom_polygon(data = ca_map,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_point(data = Rain_map_2026_summary,
             aes(x = x, y = y, color = rain),
             size = 4) +
  scale_color_viridis_c(name = "Total Rainfall") +
  coord_quickmap(
    xlim = x_range + c(-0.01, 0.01),
    ylim = y_range + c(-0.01, 0.01)
  ) +
  theme_minimal()

