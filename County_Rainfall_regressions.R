#librarys
library(ggplot2)

#Download County and Dendra data until today (1/15/2026)
Jalama_beach<-read.csv("Data/Rain/County/317dailys_Jalama_Beach_Rainfall.csv",skip=9)
Point_conception<-read.csv("Data/Rain/County/438dailys_Point_Conception.csv",skip=9)
Dendra<-read.csv("Data/Rain/All_Dendra_Rain.csv")
E5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/Escondido_5_Daily_Corrected_Jan_2026.csv")


#Make sure all files Dates are uniform
Jalama_beach$Date<-as.POSIXct(paste0(Jalama_beach$year,"-",Jalama_beach$month,"-",Jalama_beach$day),format="%Y-%m-%d")
Point_conception$Date<-as.POSIXct(paste0(Point_conception$year,"-",Point_conception$month,"-",Point_conception$day),format="%Y-%m-%d")
Dendra$Date<-as.POSIXct(Dendra$Time,format="%Y-%m-%d")
E5$Date<-as.POSIXct(E5$Date,format="%m/%d/%Y")

#Cut and rename columns
Jalama_beach<-Jalama_beach[,c(6,14)]
colnames(Jalama_beach)[1]<-"JalBeach_rain"

Point_conception<-Point_conception[,c(6,13)]
colnames(Point_conception)[1]<-"PoiConc_rain"

E5<-E5[,c(1,6)]
colnames(E5)[2]<-"E5_Rain"

comp<-merge(E5,Jalama_beach,by="Date")
comp<-merge(comp,Point_conception,by="Date")

ggplot(comp)+geom_point(aes(x=E5_Rain,y=JalBeach_rain))+theme_bw()
ggplot(comp)+geom_point(aes(x=E5_Rain,y=PoiConc_rain))+theme_bw()

##Try regrression with yearly totals (only 2 years)
data_with_wy <- addWaterYear(comp)

agg_wy<-aggregate(cbind(E5_Rain,JalBeach_rain,PoiConc_rain)~waterYear,data_with_wy,FUN=sum)
agg_wy$JBcorr<-agg_wy$JalBeach_rain/agg_wy$E5_Rain
agg_wy$PCcorr<-agg_wy$PoiConc_rain/agg_wy$E5_Rain
agg_wy


##################################################################################
#################################################################################################################
##########################################################################################################
#####Dendra#######################################################################
#remove negatives and change to inches

colnames(Dendra)<-c("Date_Time","Army","Bunker","Cistern","Cojo","Cojo_HQ",
   "Jalachichi","Jalama_HQ","Lil_cojo","N_Beach","Oaks","Quarry","Ramajal",
   "Repeater","Sutter","Tinta","Date")
#Remove sites that seem faulty on first run (Sutter,)

daily <- Dendra %>%
  group_by(Date) %>%
  summarise(
    across(-Date_Time, ~ sum(.x, na.rm = TRUE)),
    .groups = "drop" )
daily <- daily %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(.x < 0 | .x > 100, NA, .x / 25.4)))

E5$Date<-as.Date(E5$Date)
daily$Date<-as.Date(daily$Date)
daily_merge<-merge(daily,E5,by="Date")

#Break into sections to efficiently plot regression

#Estats_all_clean <- Escondido[Escondido$E2>0|Escondido$E3>0|Escondido$E5>0,]
Dendra_stats<-daily_merge[,c(2:14,16,17)] #remove date column
# Escondido Linear regression plots
D<-ggpairs(Dendra_stats,upper = "blank",diag  = "blank",
           lower = list(continuous = panel_lm_stats))
##############################################################################
##Redo with only Tinta, oaks, army camp and E5
daily_cut<-Dendra_stats[,c(1,10,14,15)]
D<-ggpairs(daily_cut,upper = "blank",diag  = "blank",
           lower = list(continuous = panel_lm_stats))
D



####################################################################################
## Aggregate Dendra data to 8am nonsense and compare to Jalama beach
Tinta<- Dendra[,c(1,16:17)]
Tinta<-Tinta %>% drop_na(Dangermond_Tinta.Rainfall.mm)
Tinta$Time<-as.POSIXct(Tinta$Time,format="%Y-%m-%d %H:%M:%S")

hourly_Tinta <- Tinta %>%
  mutate(hour_start = floor_date(Time, unit = "hour")) %>%
  group_by(hour_start) %>%
  summarize(hour_Rain = sum(Dangermond_Tinta.Rainfall.mm, na.rm = TRUE),) %>%
  ungroup()

df <- hourly_Tinta %>%
  arrange(hour_start) %>%
  mutate(
  Rain_24hr = rollapply(hour_Rain, 24, sum, fill = NA, align = "right"))

daily_rain <- df %>%
  filter(hour(hour_start) == 8)

daily_rain$Date<-as.Date(daily_rain$hour_start,format="%Y-%m-%d %H:%M:%S")
daily_rain$Tinta_rain_in<-daily_rain$Rain_24hr/25.4

Tinta_rain<-daily_rain[,4:5]
Tinta_rain
str(Jalama_beach)
Jalama_beach<-Jalama_beach[,5:6]
colnames(Jalama_beach)[1]<-"Jalama_rain"

TJ_Compare<-merge(Tinta_rain,Jalama_beach,by="Date")
ggplot(TJ_Compare)+geom_point(aes(x=Tinta_rain_in,y=Jalama_rain))+theme_bw()+ylim(0,4)+xlim(0,4)



library(ggpubr)

ggplot(TJ_Compare, aes(x = Tinta_rain_in, y = Jalama_rain)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method = "pearson", label.x.npc = "left",label.y.npc = "top") +
  stat_regline_equation(label.x.npc = "left",label.y.npc = 0.85) +theme_bw() +
  labs( x = "Tinta Rain (in)",y = "Jalama Rain (in)")+xlim(0,4)+ylim(0,4)
