library(tidyverse)
library(lubridate)
library(patchwork)
################################################################################
#E3#################
E3_well <- read.csv("Data/well/Corrected/E3_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = dtw_corrected)

# Rain data
E3_rain <- read.csv("Data/Rain/2-10-2026/JLDP Escondido 3 Rain Gauge - 20240101-20260210.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
E3 <- E3_rain %>%
  left_join(E3_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
E3 <- E3 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
E3 <- E3 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
E3_plot <- E3 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(E3_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Escondido 3 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#E2#################
E2_well <- read.csv("Data/well/Corrected/E2_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = dtw_corrected)

# Rain data
E2_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Escondido 2 Rain Gauge - 20231201-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
E2 <- E2_rain %>%
  left_join(E2_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
E2 <- E2 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
E2 <- E2 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
E2_plot <- E2 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(E2_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Escondido 2 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Gaspar 1#################
G1_well <- read.csv("Data/well/Corrected/G1_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
G1_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Tinta 4 Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
G1 <- G1_rain %>%
  left_join(G1_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
G1 <- G1 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
G1 <- G1 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
G1_plot <- G1 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(G1_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Gaspar 1 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Jalama Vaqueros#################
JV_well <- read.csv("Data/well/Corrected/JV_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
JV_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Jalama Vaqueros Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
JV <- JV_rain %>%
  left_join(JV_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
JV <- JV %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
JV <- JV %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
JV_plot <- JV %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(JV_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Jalama Vaqueros Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Oaks 3#################
O3_well <- read.csv("Data/well/Corrected/O3_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
O3_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Oaks 3B Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
O3 <- O3_rain %>%
  left_join(O3_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
O3 <- O3 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
O3 <- O3 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
O3_plot <- O3 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(O3_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Oaks3 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Oaks 5#################
O5_well <- read.csv("Data/well/Corrected/O5_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
O5_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Oaks 5 Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
O5 <- O5_rain %>%
  left_join(O5_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
O5 <- O5 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
O5 <- O5 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
O5_plot <- O5 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(O5_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Oaks 5 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#quarry 1#################
Q1_well <- read.csv("Data/well/Corrected/Q1_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
Q1_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Quarry 1 Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Q1 <- Q1_rain %>%
  left_join(Q1_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Q1 <- Q1 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Q1 <- Q1 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Q1_plot <- Q1 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(Q1_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Quarry 1 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Quail Canyon#################
Quail_well <- read.csv("Data/well/Corrected/Quail_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = dtw_corrected)
Quail_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Quail Canyon Rain Gauge - 20231231-20260212.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Quail <- Quail_rain %>%
  left_join(Quail_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Quail <- Quail %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Quail <- Quail %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Quail_plot <- Quail %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(Quail_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Quail Canyon Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p
################################################################################
#Tinta 1#################
T1_well <- read.csv("Data/well/Corrected/T1_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
T1_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Tinta 3  Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
T1 <- T1_rain %>%
  left_join(T1_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
T1 <- T1 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
T1 <- T1 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
T1_plot <- T1 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(T1_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Tinta 1 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Tinta 10#################
T10_well <- read.csv("Data/well/Corrected/T10_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = dtw_corrected)
T10_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Tinta 10 Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
T10 <- T10_rain %>%
  left_join(T10_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
T10 <- T10 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
T10 <- T10 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
T10_plot <- T10 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(T10_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Tinta 10 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Tinta 11#################
T11_well <- read.csv("Data/well/Corrected/T11B_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
T11_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Tinta 5 Rain Gauge - 20240107-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
T11 <- T11_rain %>%
  left_join(T11_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
T11 <- T11 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
T11 <- T11 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
T11_plot <- T11 %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(T11_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Tinta 11 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#TInta 5#################
T5_well <- read.csv("Data/well/Corrected/T5_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
T5_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Tinta 5 Rain Gauge - 20240107-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
T5 <- T5_rain %>%
  left_join(T5_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
T5 <- T5 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
T5 <- T5 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
T5_plot <- T5 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(T5_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Tinta 5 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Tinta 6#################
T6_well <- read.csv("Data/well/Corrected/T6_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
T6_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Tinta 6 Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
T6 <- T6_rain %>%
  left_join(T6_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
T6 <- T6 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
T6 <- T6 %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
T6_plot <- T6 %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(T6_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Tinta 6 Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

################################################################################
#Wood canyon#################
WC_well <- read.csv("Data/well/Corrected/Wood_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
WC_rain <- read.csv("Data/Rain/2-10-2026/JLDP/JLDP Oaks 1 Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
WC <- WC_rain %>%
  left_join(WC_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
WC <- WC %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
WC <- WC %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
WC_plot <- WC %>%
  filter(water_year %in% c(2024,2025, 2026))

p <- ggplot(WC_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Wood Canyon Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

###################################################################################
##Other Calgro
###################################################################################
################################################################################
#Hanson#################
Hanson_well <- read.csv("Data/well/Corrected/Hanson_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
Hanson_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Santa Clara River Aflalo Rain Gauge - 20240114-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Hanson <- Hanson_rain %>%
  left_join(Hanson_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Hanson <- Hanson %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Hanson <- Hanson %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Hanson_plot <- Hanson %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(Hanson_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Hanson Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#Aflalo#################
AF_well <- read.csv("Data/well/Corrected/AF_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
AF_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Santa Clara River Aflalo Rain Gauge - 20240114-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
AF <- AF_rain %>%
  left_join(AF_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
AF <- AF %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
AF <- AF %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
AF_plot <- AF %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(AF_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "AF Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#B and C well#################
BC_well <- read.csv("Data/well/Corrected/BC_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
BC_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Randall B and C Ranch Rain gauge - 20240106-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
BC <- BC_rain %>%
  left_join(BC_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
BC <- BC %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
BC <- BC %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
BC_plot <- BC %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(BC_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "B and C Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#Cactus#################
Cactus_well <- read.csv("Data/well/Corrected/Cactus_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
Cactus_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Randall Cactus Pasture Rain Gauge - 20240106-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Cactus <- Cactus_rain %>%
  left_join(Cactus_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Cactus <- Cactus %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Cactus <- Cactus %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Cactus_plot <- Cactus %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(Cactus_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Cactus Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#Cooper#################
Cooper_well <- read.csv("Data/well/Corrected/Coop_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = dtw_corrected)
Cooper_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Las Piletas Cooper plus Rain Gauge - 20240106-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Cooper <- Cooper_rain %>%
  left_join(Cooper_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Cooper <- Cooper %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Cooper <- Cooper %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Cooper_plot <- Cooper %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(Cooper_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Cooper Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#NE corner#################
NE_well <- read.csv("Data/well/Corrected/NE_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
NE_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Las Piletas NE Corner Rain Gauge - 20240101-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
NE <- NE_rain %>%
  left_join(NE_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
NE <- NE %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
NE <- NE %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
NE_plot <- NE %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(NE_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "NE Corner Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#Windmill#################
Windmill_well <- read.csv("Data/well/Corrected/Windmill_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
Windmill_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Las Piletas Windmill Rain Gauge - 20240106-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Windmill <- Windmill_rain %>%
  left_join(Windmill_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Windmill <- Windmill %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Windmill <- Windmill %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Windmill_plot <- Windmill %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(Windmill_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Windmill Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#Prisoners#################
Prisoners_well <- read.csv("Data/well/Corrected/Prisoners_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
Prisoners_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/SCI Prisoners Harbor well 20  Rain Gauge - 20240107-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Prisoners <- Prisoners_rain %>%
  left_join(Prisoners_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Prisoners <- Prisoners %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Prisoners <- Prisoners %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Prisoners_plot <- Prisoners %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(Prisoners_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Prisoners Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#PC#################
PC_well <- read.csv("Data/well/Corrected/PC_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
PC_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Parks Creek Rain Gauge - 20240106-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
PC <- PC_rain %>%
  left_join(PC_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
PC <- PC %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
PC <- PC %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
PC_plot <- PC %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(PC_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "PC Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p

#Pixley#################
Pixley_well <- read.csv("Data/well/Corrected/Pixley_well_daily_corrected_02_10_2026.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))%>% rename(ft_bgs = ft..below.ground.)
Pixley_rain <- read.csv("Data/Rain/2-10-2026/Other Calgro/Pixley Tulare Capinero Creek Rain Gauge - 20240107-20260211.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d (%a)")) %>% rename(rain = Rain..in.)

#Define Water Year
Pixley <- Pixley_rain %>%
  left_join(Pixley_well, by = "Date") %>%
  mutate(water_year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))

#Calculate the change in water depth
Pixley <- Pixley %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(start_date = first(Date[!is.na(ft_bgs)]),
         gw_start   = ft_bgs[Date == start_date][1],gw_change  = gw_start - ft_bgs) %>%
  ungroup()

#Calculate cumulative rainfall
Pixley <- Pixley %>% group_by(water_year) %>%
  arrange(Date) %>%
  mutate(rain = replace_na(rain, 0),cum_rain = cumsum(rain)) %>%
  ungroup()

#Filter for plot years 
Pixley_plot <- Pixley %>%
  filter(water_year %in% c(2025, 2026))

p <- ggplot(Pixley_plot, aes(Date)) +
  geom_col(aes(y = cum_rain),fill = "grey70",alpha = 0.3) +
  geom_col(aes(y = rain),fill = "steelblue") +
  geom_line(aes(y = gw_change),linewidth = 1) +
  facet_wrap(~ water_year, nrow = 1, scales = "free_x") +labs(
    x = NULL,y = "Rainfall / GW Change",
    title = "Rainfall, Cumulative Rainfall, and Groundwater Response",
    subtitle = "Pixley Well") +
  theme_bw() + theme( strip.text = element_text(size = 12, face = "bold"))
p
