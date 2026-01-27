#Not ready for full analysis
#For now- Calculate 5 year, 10 year and 12 year monthly cumulative rainfall for jalama beach
#Then calculate the monthly total rainfall for 2024, 2025 and 2026 at Jalama beach
#Then calculate the deviation from mean for each month/ year 2024-2025
#Need to impute data for 2026 water year
#Calculate monthly average change in water depthe and monthly change in water depth
#
Jalama_beach<-read.csv("Data/Rain/County/317dailys_Jalama_Beach_Rainfall.csv",skip=9)
Jalama_beach<-Jalama_beach[,2:6]
Jalama_beach$Date<-as.Date(paste0(Jalama_beach$year,"-",Jalama_beach$month,"-",Jalama_beach$day),format="%Y-%m-%d")

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

JB_cumulative <- monthly_complete %>% 
  filter(water_year>="2010"&water_year<="2022")%>% 
  group_by(water_year) %>% 
  mutate(sumrain=cumsum(monthly_rain))

mmc<-"meam_monthly_cumulative"

JB_mmc_12y<- JB_cumulative %>% 
  group_by(month) %>% 
  summarise(meansumrain=mean(sumrain),
            meanmonthrain=mean(monthly_rain),
            sd_meansumrain= sd(sumrain),
            sd_meanmonthrain=sd(monthly_rain))


#add in ranchbot data
E5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/Escondido_5_Daily_Corrected_Jan_2026.csv")
E5$Date<-as.Date(E5$Date,format="%m/%d/%Y")
#E5<-addWaterYear(E5)
E5$month<-format(E5$Date,format="%m")
E5$year<-format(E5$Date,format="%Y")
str(E5)
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
           fill = list(monthly_rain = 0) ) %>%
  mutate(year  = year(ym),month = month(ym),
         water_year = if_else(month >= 9,year + 1L,year))

E5_cumulative_24 <- monthly_complete_E5 %>% 
  filter(water_year==2024) %>% 
  group_by(water_year) %>% 
  mutate(sumrain=cumsum(monthly_rain))

E5_forMerge_24<-E5_cumulative_24[,c(2,3,5,7)]
colnames(E5_forMerge_24)<-c("monthly_rain_2024","mean_wl_2024","month","sumrain2024")

E5_cumulative_25 <- monthly_complete_E5 %>% 
  filter(water_year==2025) %>% 
  group_by(water_year) %>% 
  mutate(sumrain=cumsum(monthly_rain))

E5_forMerge_25<-E5_cumulative_25[,c(2,3,5,7)]
colnames(E5_forMerge_25)<-c("monthly_rain_2025","mean_wl_2025","month","sumrain2025")
all<-merge(E5_forMerge_24,JB_mmc_12y,by="month")

all<-merge(E5_forMerge_25,all,by="month")

ggplot(all)+geom_col(aes(x=month,y=meansumrain))



wy_month_order <- c(9,10,11,12,1,2,3,4,5,6,7,8)

monthly_complete <- all %>%
  mutate(wy_month = factor(
      month,levels = c(9,10,11,12,1,2,3,4,5,6,7,8),
      labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr",
        "May","Jun","Jul","Aug")))

ggplot(monthly_complete, aes(x = wy_month, y = meansumrain)) +
  geom_col() +
  labs(x = "Water Year Month", y = "Monthly Rainfall")

ggplot(monthly_complete, aes(x = wy_month, y = meansumrain)) +
  geom_col() +
  labs(x = "Water Year Month", y = "Monthly Rainfall")
