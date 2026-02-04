#Not ready for full analysis
#For now- Calculate 5 year, 10 year and 12 year monthly cumulative rainfall for jalama beach
#Then calculate the monthly total rainfall for 2024, 2025 and 2026 at Jalama beach
#Then calculate the deviation from mean for each month/ year 2024-2025
#Need to impute data for 2026 water year
#Calculate monthly average change in water depthe and monthly change in water depth
#
library(cowplot)
library(slider)
Jalama_beach<-read.csv("Data/Rain/County/317dailys_Jalama_Beach_Rainfall.csv",skip=9)
Jalama_beach<-Jalama_beach[,2:6]
Jalama_beach$Date<-as.Date(paste0(Jalama_beach$year,"-",Jalama_beach$month,"-",Jalama_beach$day),format="%Y-%m-%d")

#Filling in 0 for missing days and summing monthly rainfall
monthly_complete <- Jalama_beach %>%
  filter(!is.na(year), !is.na(month)) %>%
  mutate(ym = make_date(year, month, 1)) %>%
  group_by(ym) %>%
  summarise(monthly_rain = sum(daily.rain, na.rm = TRUE),
    .groups = "drop") %>%
  complete(ym = seq(min(ym), max(ym), by = "1 month"),
    fill = list(monthly_rain = 0) ) %>%
  mutate(year  = year(ym),month = month(ym),
    water_year = if_else(month >= 9,year + 1L,year))

#cumulative wter year
JB_cumulative <- monthly_complete %>% 
  filter(water_year>="2010"&water_year<="2022")%>% 
  group_by(water_year) %>% 
  mutate(sumrain=cumsum(monthly_rain))

mmc<-"mean_monthly_cumulative"

JB_mmc_12y<- JB_cumulative %>% 
  group_by(month) %>% 
  summarise(meansumrain=mean(sumrain),
            meanmonthrain=mean(monthly_rain),
            sd_meansumrain= sd(sumrain),
            sd_meanmonthrain=sd(monthly_rain))

#add in ranchbot data
E5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/Escondido_5_Daily_Corrected_Jan_2026.csv")
E5$Date<-as.Date(E5$Date,format="%m/%d/%Y")
E5$month<-format(E5$Date,format="%m")
E5$year<-format(E5$Date,format="%Y")
E5$month<-as.numeric(E5$month)
E5$year<-as.numeric(E5$year)

monthly_complete_E5 <- E5 %>%
  filter(!is.na(year), !is.na(month)) %>%
  mutate(ym = make_date(year, month, 1)) %>%
  group_by(ym) %>%
  summarise(monthly_rain = sum(Rain_in, na.rm = TRUE),
  mean_WL=mean(Corrected.mean.ft.below.ground,na.rm=TRUE),
            .groups = "drop") %>%
  complete(ym = seq(min(ym), max(ym), by = "1 month"),
  fill = list(monthly_rain = 0) ) %>%mutate(year  = year(ym),
  month = month(ym),water_year = if_else(month >= 9,year + 1L,year))

E5_cumulative_24 <- monthly_complete_E5 %>% 
  filter(water_year==2024) %>% group_by(water_year) %>% 
  mutate(sumrain=cumsum(monthly_rain))

E5_forMerge_24<-E5_cumulative_24[,c(2,3,5,7)]
colnames(E5_forMerge_24)<-c("monthly_rain_2024","mean_wl_2024","month","sumrain2024")

E5_cumulative_25 <- monthly_complete_E5 %>% 
  filter(water_year==2025) %>% group_by(water_year) %>% 
  mutate(sumrain=cumsum(monthly_rain))

E5_forMerge_25<-E5_cumulative_25[,c(2,3,5,7)]
colnames(E5_forMerge_25)<-c("monthly_rain_2025","mean_wl_2025","month","sumrain2025")
all<-merge(E5_forMerge_24,JB_mmc_12y,by="month")
all<-merge(E5_forMerge_25,all,by="month")

wy_month_order <- c(9,10,11,12,1,2,3,4,5,6,7,8)

monthly_complete <- all %>%
  mutate(wy_month = factor(
      month,levels = c(9,10,11,12,1,2,3,4,5,6,7,8),
      labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr",
        "May","Jun","Jul","Aug")))

ggplot(monthly_complete, aes(x = wy_month, y = meansumrain)) +
  geom_col() +labs(x = "Water Year Month", y = "Monthly Rainfall")

ggplot(monthly_complete, aes(x = wy_month, y = meansumrain)) +
  geom_col() +labs(x = "Water Year Month", y = "Monthly Rainfall")

##################################################################################
###Trying with Dendra Tinta rainfall data based on regression results############
Dendra<-read.csv("Data/Rain/All_Dendra_Rain.csv")
E5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/Escondido_5_Daily_Corrected_Jan_2026.csv")
Dendra$Date<-as.POSIXct(Dendra$Time,format="%Y-%m-%d")

Tinta<-Dendra[,16:17]
colnames(Tinta)[1]<-"tinta"
Tinta <- Tinta %>% drop_na(tinta)
Tinta_Daily <- Tinta %>%
  group_by(Date) %>%
  summarise(tinta_rain=sum(tinta))

Tinta_imperial <- Tinta_Daily %>%
  mutate(across(where(is.numeric),
      ~ ifelse(.x < 0 | .x > 100, NA, .x / 25.4)))

#Filling in 0 for missing days and summing monthly rainfall
Tinta_imperial$month<-format(Tinta_imperial$Date,format="%m")
Tinta_imperial$Year<-format(Tinta_imperial$Date,format="%Y")

monthly_complete <- Tinta_imperial %>%
  filter(!is.na(Year), !is.na(month)) %>%
  mutate(ym = make_date(Year, month, 1)) %>%
  group_by(ym) %>%
  summarise(monthly_rain = sum(tinta_rain, na.rm = TRUE),
            .groups = "drop") %>%
  complete(ym = seq(min(ym), max(ym), by = "1 month"),
           fill = list(monthly_rain = 0) ) %>%
  mutate(year  = year(ym),month = month(ym),
         water_year = if_else(month >= 9,year + 1L,year))

wy_month_order <- c(9,10,11,12,1,2,3,4,5,6,7,8)

monthly_complete <- monthly_complete %>%
  mutate(wy_month = factor(
    month,levels = c(9,10,11,12,1,2,3,4,5,6,7,8),
    labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr",
               "May","Jun","Jul","Aug")))

month_mean<-monthly_complete %>% 
  group_by(wy_month) %>% 
  summarise(monthly_mean=mean(monthly_rain))

#moving mean calculation and plotting
monthly_complete <- monthly_complete %>%
  arrange(ym) %>% mutate(rain_24mo_mean = slide_dbl(monthly_rain,
      mean,.before = 23,.complete = TRUE) )

monthly_complete <- monthly_complete %>%
  mutate(rain_24mo_sum = slide_dbl(
      monthly_rain,sum,.before = 23,.complete = TRUE ))

ggplot(monthly_complete, aes(x = ym)) +
  geom_col(aes(y = monthly_rain), alpha = 0.6) +
  geom_line(aes(y = rain_24mo_mean), linewidth = 1) +
  labs( x = "Date",y = "Monthly Rainfall",
    title = "Monthly Rainfall with 24-Month Rolling Mean"
  ) +theme_minimal()

###Attempting with cumulative data
monthly_complete<-monthly_complete[monthly_complete$ym>="2021-09-01",]
monthly_complete <- monthly_complete %>%
  arrange(ym) %>%group_by(water_year) %>%
  mutate(wy_cum_rain = cumsum(monthly_rain)) %>%
  ungroup()

###########Probably delete... Plotting all water years to one average
##manually compute for each year
monthly_complete <- monthly_complete %>%
  mutate(wy_month_num = if_else(month >= 9, month - 8L, month + 4L))

last_wys_24_25 <- sort(unique(monthly_complete$water_year), decreasing = TRUE)[2:3]

wy_climo <- monthly_complete %>%
  filter(water_year %in% last_wys_24_25) %>%
  group_by(wy_month_num) %>%
  summarise(climo_cum = mean(wy_cum_rain, na.rm = TRUE),
    .groups = "drop")

monthly_complete_26<-monthly_complete[monthly_complete$water_year==2026,]

monthly_complete_26 <- monthly_complete_26 %>%
  left_join(wy_climo, by = "wy_month_num")

c<-ggplot(monthly_complete_26, aes(x = wy_month_num)) +
  geom_line(aes(y = wy_cum_rain), alpha = 0.5,linewidth = 1.2,color="lightblue") +
  geom_line(aes(y = climo_cum), linewidth = 1.3) +
  scale_x_continuous(breaks = 1:12,labels = c("Sep","Oct","Nov",
  "Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug")) +
  labs(x = "Water Year Month",y = "Cumulative Rainfall",
  title = "2026 Cumulative Rainfall vs 2-Year Average")
  +ylim(0,37)+ theme_minimal()

#25
last_wys_23_24 <- sort(unique(monthly_complete$water_year), decreasing = TRUE)[3:4]

wy_climo <- monthly_complete %>%
  filter(water_year %in% last_wys_23_24) %>%
  group_by(wy_month_num) %>%
  summarise(climo_cum = mean(wy_cum_rain, na.rm = TRUE),
    .groups = "drop")

monthly_complete_25<-monthly_complete[monthly_complete$water_year==2025,]
monthly_complete_25 <- monthly_complete_25 %>%
  left_join(wy_climo, by = "wy_month_num")

b<-ggplot(monthly_complete_25, aes(x = wy_month_num)) +
  geom_line(aes(y = wy_cum_rain), alpha = 0.5,
  linewidth = 1.2,color="lightblue") +
  geom_line(aes(y = climo_cum), linewidth = 1.3) +
  scale_x_continuous(breaks = 1:12,labels = c("Sep","Oct","Nov",
  "Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug")) +
  labs( x = "Water Year Month",y = "Cumulative Rainfall",
    title = "2025 Cumulative Rainfall vs 2-Year Average"
  ) +ylim(0,37)+theme_minimal()

#24
last_wys_22_23 <- sort(unique(monthly_complete$water_year), decreasing = TRUE)[4:5]

wy_climo <- monthly_complete %>%
  filter(water_year %in% last_wys) %>%
  group_by(wy_month_num) %>%
  summarise(climo_cum = mean(wy_cum_rain, na.rm = TRUE),
    .groups = "drop")

monthly_complete_24<-monthly_complete[monthly_complete$water_year==2024,]
monthly_complete_24 <- monthly_complete_24 %>%
  left_join(wy_climo, by = "wy_month_num")

a<-ggplot(monthly_complete_24, aes(x = wy_month_num)) +
  geom_line(aes(y = wy_cum_rain, group = water_year,color=water_year), alpha = 0.5,
  linewidth = 1.2,color="lightblue") +
  geom_line(aes(y = climo_cum), linewidth = 1.3) +
  scale_x_continuous(breaks = 1:12,labels = c("Sep","Oct","Nov",
  "Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug")) +
  labs(x = "Water Year Month",y = "Cumulative Rainfall",
  title = "2024 Cumulative Rainfall vs 2-Year Average")+
  ylim(0,37)+theme_minimal()

plot_grid(a,b,c, nrow = 1, align = "hv", axis = "l")+title("Rain")

################################################################################
### NEW###############################################################################
################################################################################
#Now import Escondido and calculate monthly change in water depth 
#From 1st of month to last of month

E5
str(E5)
plot(E5$Corrected.mean.ft.below.ground~E5$Date)
