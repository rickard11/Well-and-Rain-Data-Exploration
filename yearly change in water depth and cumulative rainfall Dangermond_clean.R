#Clean and automate hydrographs
library(tidyverse)
library(dplyr)
library(ggplot2)
#Read in all well data
folder_path <- "Data/well/Corrected/"
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
get_well_name <- function(filename) {
  name <- basename(filename)
  name <- str_remove(name, "_well.*")
  return(name)}
  
well_data <- files %>%
  set_names(map_chr(., get_well_name)) %>%
  map(read_csv)

well_data <- map_dfr(files, ~ read_csv(.x) %>%
                       mutate(Name = get_well_name(.x)))

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

#Merge data and calculate stats
Site_data<-merge(rain_data,well_data,by=c("Date","Name"))

#Define Water Year
Site_data <- Site_data %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Site_data <- Site_data %>% group_by(water_year,Name) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft..below.ground.)]),
         gw_start   = ft..below.ground.[Date == start_date][1],
         gw_change  = gw_start - ft..below.ground.) %>%
         ungroup()

#Calculate cumulative rainfall
Site_data <- Site_data %>% group_by(water_year,Name) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(`Rain (in)`, 0),cum_rain = cumsum(`Rain (in)`)) %>%
  ungroup()


for (i in unique(Site_data$Name)){
  p<-Site_data[Site_data$Name==i,]
  q<- ggplot(p, aes(Date)) +
    geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
    geom_col(aes(y = rain),fill = "steelblue") +
    geom_line(aes(y = gw_change),linewidth = 1) +
    facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
      x = NULL,y = "Rainfall / GW Change",
      title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
      subtitle = i) + theme_bw()+  
    scale_x_date(date_labels = "%b", date_breaks = "2 month")+
    theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"),
                        axis.title = element_text(size = 16),
                        axis.text = element_text(size=12))
  print(q)
  filename <- paste0("my_plot_", i, ".png")
  ggsave(filename = filename, plot = q)
}



# Find average depth of first week and average depth at last week of each water year and site and make a new table

Site_summary <- Site_data %>%
  arrange(Name, water_year, Date) %>%   # make sure data are ordered
  group_by(Name, water_year) %>%
  mutate(first_day = min(Date, na.rm = TRUE),
    last_day  = max(Date, na.rm = TRUE)) %>%
  summarise(first_week_avg = mean(
      ft..below.ground.[Date <= first_day + 6],na.rm = TRUE),
    last_week_avg = mean(
      ft..below.ground.[Date >= last_day - 6],na.rm = TRUE),
    Sum_Rain=sum(`Rain (in)`),
    .groups = "drop")

Site_summary$est_recharge<-round(((Site_summary$first_week_avg-Site_summary$last_week_avg)*0.1)*12,digits=1)
Site_summary$p_recharge<-round((Site_summary$est_recharge/Site_summary$Sum_Rain)*100,digits=1)

