library(ggplot2)
library(cowplot)
library(patchwork)
gav_gnss<-read.csv("Data/GNSS/tjrn_enu_positions.csv")
gav_gnss$Date <- as.Date(gav_gnss$doy - 1, origin = paste0(gav_gnss$year, "-01-01"))

E5_well<-read.csv("Data/GNSS/Escondido 5 20231001-20260128 Corrected.csv")
E5_well$Date<-as.Date(E5_well$Date,format="%m/%d/%Y")


gnss_E5<-merge(gav_gnss,E5_well,by="Date",all.x=TRUE,all.y=TRUE)
str(gnss_E5)
gnss_E5_clean<-gnss_E5[,c(1,6,11,14)]

coeff <- 5
range(gnss_E5$Date)
#2 seperate water years
gnss_E5_24<-gnss_E5_clean[gnss_E5_clean$Date<="2024-05-01"&gnss_E5_clean>="2023-09-01",]
gnss_E5_24$gw_change<-gnss_E5_24$Corrected.mean.ft.below.ground-60.99
gnss_E5_24$cum_rain<-cumsum(replace(gnss_E5_24$Rain_in,is.na(gnss_E5_24$Rain_in),0))

gnss_E5_25<-gnss_E5_clean[gnss_E5_clean$Date<="2025-05-01"&gnss_E5_clean>="2024-09-01",]
gnss_E5_25$gw_change<-gnss_E5_25$Corrected.mean.ft.below.ground-58.18
gnss_E5_25$cum_rain<-cumsum(replace(gnss_E5_25$Rain_in,is.na(gnss_E5_25$Rain_in),0))

gnss_E5_26<-gnss_E5_clean[gnss_E5_clean$Date>="2025-09-01",]
gnss_E5_26$gw_change<-gnss_E5_26$Corrected.mean.ft.below.ground-64.49
gnss_E5_26$cum_rain<-cumsum(replace(gnss_E5_26$Rain_in,is.na(gnss_E5_26$Rain_in),0))

a<-ggplot(gnss_E5_24, aes(x=Date)) +
  geom_col(aes(y=cum_rain/coeff),color="grey",fill="grey")+
  geom_col( aes(y=Rain_in),color="blue") + 
  geom_point( aes(y=dU / coeff),alpha=0.5) +
  geom_line(aes(y=gw_change))+
  scale_y_continuous(limits=c(-15,7.5),name = "Rain Inches",
  sec.axis = sec_axis(~.*coeff, name="GNSS upward movement (mm)"))+
  theme_bw()+ggtitle("2024 Water Year")

b<-ggplot(gnss_E5_25, aes(x=Date)) +
  geom_col(aes(y=cum_rain/coeff),color="grey",fill="grey")+
  geom_col( aes(y=Rain_in),color="blue") + 
  geom_point( aes(y=dU / coeff),alpha=0.3) +
  geom_line(aes(y=gw_change))+
  scale_y_continuous(limits=c(-15,7.5),name = "Rain Inches",
  sec.axis = sec_axis(~.*coeff, name="GNSS upward movement (mm)"))+
  theme_bw()+ggtitle("2025 Water Year")

c<-ggplot(gnss_E5_26, aes(x=Date)) +
  geom_col(aes(y=cum_rain/coeff),color="grey",fill="grey")+
  geom_col( aes(y=Rain_in),color="blue") + 
  geom_point( aes(y=dU / coeff),alpha=0.3) +
  geom_line(aes(y=gw_change))+
  scale_y_continuous(limits=c(-15,7.5),name = "Rain Inches",
  sec.axis = sec_axis(~.*coeff, name="GNSS upward movement (mm)"))+
  theme_bw()+ggtitle("2026 Water Year")


plot_grid(a,b,c,ncol=3)

tail(gnss_E5_26)

#######################################################
##Remove GNSS and reverse change

#2 seperate water years
gnss_E5_24$gw_change<-60.99-gnss_E5_24$Corrected.mean.ft.below.ground
gnss_E5_25$gw_change<-58.18-gnss_E5_25$Corrected.mean.ft.below.ground
gnss_E5_26$gw_change<-64.49-gnss_E5_26$Corrected.mean.ft.below.ground

a<-ggplot(gnss_E5_24, aes(x=Date)) +
  geom_col(aes(y=cum_rain),color="grey",alpha=0.15)+
  geom_col( aes(y=Rain_in),color="blue") + 
  geom_line(aes(y=gw_change),linewidth=1)+ylim(-5,36)+
  theme_bw()+ggtitle("2024 Water Year")

b<-ggplot(gnss_E5_25, aes(x=Date)) +
  geom_col(aes(y=cum_rain),color="grey",alpha=0.15)+
  geom_col( aes(y=Rain_in),color="blue") + 
  geom_line(aes(y=gw_change),linewidth=1)+
  ylim(-5,36)+theme_bw()+ggtitle("2025 Water Year")

c<-ggplot(gnss_E5_26, aes(x=Date))+
  geom_col(aes(y=cum_rain),color="grey",alpha=0.15)+
  geom_col( aes(y=Rain_in),color="blue") + 
  geom_line(aes(y=gw_change),linewidth=1)+ylim(-5,36)+
  theme_bw()+ggtitle("2026 Water Year")

plot_grid(a,b,c,ncol=3)

gnss_E5$rain_mm<-gnss_E5$Rain_in*25.4
tail(gnss_E5)
ggplot(gnss_E5, aes(x=Date))+
  geom_col( aes(y=rain_mm),color="blue") + 
  geom_line(aes(y=Corrected.mean.ft.below.ground),linewidth=1)+
  theme_bw()+ggtitle("Escondido 5 Well Depth to Water and Daily Rainfall")+
  scale_y_reverse()

####Or 2 seperate plots 

a<-ggplot(gnss_E5, aes(x=Date))+ 
  geom_line(aes(y=Corrected.mean.ft.below.ground),linewidth=1)+
  theme_bw()+ scale_y_reverse()+ylab("Depth to Water (Feet)")

b<-ggplot(gnss_E5, aes(x=Date))+
  geom_col( aes(y=Rain_in),color="cornflowerblue") +
  theme_bw()+ggtitle("Escondido 5 Depth to Water and Daily Rainfall")+
  scale_y_reverse()+theme(axis.title.x = element_blank(),
                          axis.text.x = element_blank())+
  ylab("Rain (Inches)")

b / a + plot_layout(heights = c(1, 3))

ggsave("figures/Escondido_5_Timeseries.png")
