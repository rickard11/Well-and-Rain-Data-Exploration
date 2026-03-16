# well data cleaning and automation
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read in all well data
folder_path <- "Data/well/2-11-2026/"
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
colnames(well_data)<-c("Date_Time","ft_below_ground","Name")

# Need to adjust Names to match the corrections dataset first before applying function
unique(well_data$Name)
well_data$Name<-gsub("JLDP ", "", well_data$Name)
well_data$Name<-gsub("Las Piletas ", "", well_data$Name)
well_data$Name<-gsub("Randall ", "", well_data$Name)
well_data$Name<-gsub("Santa Clara River ", "", well_data$Name)
well_data$Name<-gsub("SCI ", "", well_data$Name)

well_data$Name <- sub(" Well.*", "", well_data$Name)
well_data$Name <- sub(" -.*", "", well_data$Name)
well_data$Name <- sub(" plus.*", "", well_data$Name)
well_data$Name <- sub("well.*", "", well_data$Name)

#Read in well corrections
corrections<-read.csv("Data/well/Corrections.csv")
corrections$Start_Date<-as.POSIXct(paste0(corrections$Start_Date," 01:00"),format="%m/%d/%Y %H:%M")
corrections$End_Date<-as.POSIXct(paste0(corrections$End_Date," 01:00"),format="%m/%d/%Y %H:%M")

#Apply Corrections
df_corrected <- well_data %>%
  group_by(Name) %>%
  filter(as.Date(Date_Time) != min(as.Date(Date_Time))) %>%
  ungroup() %>%
  left_join(corrections, by = "Name") %>%
  mutate(End_Date = if_else(is.na(End_Date), as.POSIXct("2100-01-01"), End_Date)) %>%
  mutate(End_Date = if_else(is.na(End_Date),as.POSIXct("2100-01-01", tz = tz(Date_Time)), End_Date),
         offset_applied = if_else(
           !is.na(Start_Date) &
             Date_Time >= Start_Date &
             Date_Time <= End_Date,
           offset,0)) %>%
  group_by(Name, Date_Time) %>%
  summarise(
    ft_below_ground = first(ft_below_ground),
    ft_corrected = ft_below_ground + sum(offset_applied),
    .groups = "drop")

#NEW- not working to delete desired data
df_corrected <- well_data %>%
  group_by(Name) %>%
  filter(as.Date(Date_Time) != min(as.Date(Date_Time))) %>%
  ungroup() %>%
  left_join(corrections, by = "Name", relationship = "many-to-many") %>%
  mutate(
    End_Date = coalesce(End_Date, as.POSIXct("2100-01-01", tz = tz(Date_Time))),
    in_window = !is.na(Start_Date) &
      Date_Time >= Start_Date &
      Date_Time <= End_Date
  ) %>%
  group_by(Name, Date_Time) %>%
  summarise(
    ft_below_ground = first(ft_below_ground),
    
    remove_flag = any(in_window & action == "Remove"),
    
    offset_total = sum(if_else(in_window & action == "offset", offset, 0), na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(!remove_flag) %>%
  mutate(ft_corrected = ft_below_ground + offset_total)

well_data %>% nrow()
df_corrected %>% nrow()
#Plot each example to check for bad data
for (i in unique(df_corrected$Name)){
  p<-df_corrected[df_corrected$Name==i,]
  q<- ggplot(p, aes(Date_Time)) +
    geom_line(aes(y = ft_corrected),linewidth = 1)+
    ggtitle(i)
  print(q)
}

write.csv(df_corrected,"Data/processed/Calgro_Well_data_Corrected_2_10_2026.csv")
