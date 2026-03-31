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

#merge the raw and corrected data
Site_data <- Site_data %>%
  mutate(dtw_final = coalesce(dtw_corrected, ft..below.ground.))

#Calculate the change in water depth
Site_data <- Site_data %>% group_by(water_year,Name) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(dtw_final)]),
         gw_start   = dtw_final[Date == start_date][1],
         gw_change  = gw_start - dtw_final) %>%
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
    theme( strip.text = element_text(size = 12, face = "bold"),
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
write.cs
Site_data_clean<-Site_data[Site_data$Name!="Pixley Tulare Capinero Creek"&
                             Site_data$Name!="Las Piletas Cooper plus"&
                             Site_data$Name!="Randall Lower Tweedy"&
                             Site_data$Name!="Randall Cactus Pasture"&
                             Site_data$Name!="JLDP Tinta 10",]

A<-ggplot(Site_data_clean, aes(Date)) +
  geom_line(aes(y = gw_change, color=Name),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response") + theme_bw()+  
  scale_x_date(date_labels = "%b", date_breaks = "2 month")+
  theme( strip.text = element_text(size = 12, face = "bold"),
                      axis.title = element_text(size = 16),
                      axis.text = element_text(size=12))

ggsave("figures/Calgro_GW_response.png",A)

Site_data_JLDP<-Site_data[Site_data$Name=="JLDP Escondido 2"|
                             Site_data$Name=="JLDP Escondido 3"|
                             Site_data$Name=="JLDP Tinta 5"|
                             Site_data$Name=="JLDP Tinta 6"|
                             Site_data$Name=="JLDP Oaks 5",]

J<-ggplot(Site_data_JLDP, aes(Date)) +
  geom_line(aes(y = gw_change, color=Name),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "GW Change",
    title = "Groundwater Response") + theme_bw()+  
  scale_x_date(date_labels = "%b", date_breaks = "2 month")+
  theme( strip.text = element_text(size = 12, face = "bold"),
                      axis.title = element_text(size = 16),
                      axis.text = element_text(size=12))

ggsave("figures/JLDP_GW_response.png",J)
JLDP_rain_summary<-aggregate(`Rain (in)`~Name + water_year,Site_data_JLDP,FUN=sum)

JLDP_rain_summary$water_year<-as.character(JLDP_rain_summary$water_year)

JR<-ggplot(JLDP_rain_summary, aes(x=water_year)) +
  geom_boxplot(aes(y=`Rain (in)`)) +
  geom_jitter(aes(y =`Rain (in)`, color=Name))+labs(
    title = "Yearly Rainfall") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"),
                      axis.title = element_text(size = 16),
                      axis.text = element_text(size=12))
ggsave("figures/JLDP_Yearly_rainfall.png",JR)
