Escondido<-read.csv("Data/processed/Escondido_rain_auto_flag.csv")
Oaks<-read.csv("Data/processed/Oaks_rain_auto_flag.csv")
Tinta<-read.csv("Data/processed/Tinta_rain_auto_flag.csv")

#Download new data
E2<-read.csv("Data/Rain/New rain/JLDP Escondido 2 Rain Gauge Rain Gauge 20231001-20260119.csv")
E3<-read.csv("Data/Rain/New rain/JLDP Escondido 3 Rain Gauge Rain Gauge 20231001-20260119.csv")
E5<-read.csv("Data/Rain/12-1-2025 CalGro Download/JLDP/Escondido_5_Daily_Corrected_Jan_2026.csv")
O3B<-read.csv("Data/Rain/New rain/JLDP Oaks 3B Rain Gauge Rain Gauge 20231001-20260119.csv")
O4<-read.csv("Data/Rain/New rain/JLDP Oaks 4 Rain Gauge Rain Gauge 20231001-20260119.csv")
O5<-read.csv("Data/Rain/New rain/JLDP Oaks 5 Rain Gauge Rain Gauge 20231001-20260119.csv")
Q<-read.csv("Data/Rain/New rain/JLDP Quail Canyon Rain Gauge Rain Gauge 20251001-20260119.csv")
T3<-read.csv("Data/Rain/New rain/JLDP Tinta 3  Rain Gauge Rain Gauge 20231001-20260119.csv")
T4<-read.csv("Data/Rain/New rain/JLDP Tinta 4 Rain Gauge Rain Gauge 20231001-20260119.csv")
T5<-read.csv("Data/Rain/New rain/JLDP Tinta 5 Rain Gauge Rain Gauge 20231001-20260119.csv")
T6<-read.csv("Data/Rain/New rain/JLDP Tinta 6 Rain Gauge Rain Gauge 20231001-20260119.csv")

#
E2
E3
E5<-E5[,c(1,6)]
colnames(E5)[2]<-"E5"
colnames(E2)[2]<-"E2"
colnames(E3)[2]<-"E3"
colnames(O3B)[2]<-"O3B"
colnames(O4)[2]<-"O4"
colnames(O5)[2]<-"O5"
colnames(Q)[2]<-"Q"
colnames(T3)[2]<-"T3"
colnames(T4)[2]<-"T4"
colnames(T5)[2]<-"T5"
colnames(T6)[2]<-"T6"

df_list <- list(E2,E3,O3B,O4,O5,Q,T3,T4,T5,T6) 
all <- df_list %>% reduce(full_join, by = "Date")
all$Date<-as.Date(all$Date,format="%Y-%m-%d (%a)")

E5$Date<-as.Date(E5$Date,format="%m/%d/%Y")
all<-merge(all,E5,by="Date")

all<-addWaterYear(all)
all26<-all[all$waterYear==2026,]
#all26$meanE<-mean(all$E2,all$E3,all$E5)
all26$sumE2<-cumsum(all26$E2)
all26$sumE3<-cumsum(all26$E3)
all26$sumE5<-cumsum(all26$E5)
all26$sumO3B<-cumsum(all26$O3B)
all26$sumO4<-cumsum(all26$O4)
all26$sumO5<-cumsum(all26$O5)
all26$sumQ<-cumsum(all26$Q)
all26$sumT3<-cumsum(all26$T3)
all26$sumT4<-cumsum(all26$T4)
all26$sumT5<-cumsum(all26$T5)
all26$sumT6<-cumsum(all26$T6)



all_cumulative<-all26[,c(1,14:24)]
data_long26 <- all_cumulative %>%
  pivot_longer(
    cols = !Date, # Select columns starting with "temp_"
    names_to = "site",           # Name the new key column "year"
    values_to = "cumulative_rainfall"    # Name the new value column "temperature"
  )

sumrain<-ggplot(data_long26,aes(x=Date))+geom_line(aes(y=cumulative_rainfall,color=site))+
  theme_bw()

ggsave(plot=sumrain,"figures/Cumulative_Rainfall.png")


datasnip<-data_long26[data_long26$site=="sumT6"|
          data_long26$site=="sumT5"|data_long26$site=="sumE3",]

ggplot(datasnip,aes(x=Date))+geom_line(aes(y=cumulative_rainfall,
  color=site),linewidth =1.2,alpha=0.7)+theme_bw()
