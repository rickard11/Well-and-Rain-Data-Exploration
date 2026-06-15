library(patchwork)
library(ggplot2)
E5_new<-read.csv("Data/well/Corrected/JLDP Escondido 3_well_daily_corrected_02_10_2026.csv")
E5_new$Date<-as.Date(E5_new$Date,format="%Y-%m-%d")

E5_new_daily<-aggregate(dtw_corrected~Date,E5_new,FUN=mean)

E5_rain<-read.csv("Data/Rain/New rain/JLDP Escondido 3 Rain Gauge Rain Gauge 20231001-20260119.csv")
E5_rain<-E5_rain[,c(1,6)]
E5_rain$Date<-as.Date(E5_rain$Date,format="%Y-%m-%d (%a)")

E5_all<-merge(E5_rain,E5_new_daily,by="Date")
tail(E5_all)
head(E5_all)
####Or 2 seperate plots 
b<-ggplot(E5_all, aes(x=Date))+
  geom_col( aes(y=Rain..in.),color="cornflowerblue") +
  theme_bw()+ggtitle("Escondido 3 Depth to Water and Daily Rainfall")+
  scale_y_reverse()+theme(axis.title = element_text(size=14,face="bold",color = "black"),
                          axis.text = element_text(size=14,face = "bold",color="black"),
                          axis.title.x = element_blank(),
                          axis.text.x = element_blank(),
                          title = element_text(size=14,face="bold",color="black"))+
                          ylab("Rain (Inches)")

a<-ggplot(E5_all, aes(x=Date))+ 
  geom_line(aes(y=dtw_corrected),linewidth=1)+
  theme_bw()+ scale_y_reverse()+ylab("Depth to Water (Feet)")+
  theme(axis.title = element_text(size=14,face="bold", color="black") ,
        axis.text = element_text(size=14,face="bold",color="black"))

b / a + plot_layout(heights = c(1, 3))


E5_new<-read.csv("Data/well/Corrected/JLDP Escondido 3_well_daily_corrected_02_10_2026.csv")
E5_new$Date<-as.Date(E5_new$Date.and.Time,format="%m/%d/%Y")
E5_new$Date<-format(E5_new$Date.and.Time,format="%Y-%m-%d")
E5_new$Date<-as.Date(E5_new$Date)
E5_new_daily<-aggregate(ft..below.ground.~Date,E5_new,FUN=mean)

E5_rain<-read.csv("Data/Rain/2-10-2026/JLDP Jalama Vaqueros Rain Gauge - 20240101-20260211.csv")
E5_rain<-E5_rain[,c(1,6)]
E5_rain$Date<-as.Date(E5_rain$Date,format="%Y-%m-%d (%a)")

E5_all<-merge(E5_rain,E5_new_daily,by="Date")
tail(E5_all)
head(E5_all)
####Or 2 seperate plots 
?theme

b<-ggplot(E5_all, aes(x=Date))+
  geom_col( aes(y=Rain..in.),color="cornflowerblue") +
  theme_bw()+ggtitle("Escondido 3 Depth to Water and Daily Rainfall")+
  scale_y_reverse()+theme(axis.title = element_text(size=15,face="bold",color = "black"),
                          axis.text = element_text(size=15,face = "bold",color="black"),
                          axis.title.x = element_blank(),
                          axis.text.x = element_blank(),
                          title = element_text(size=15,face="bold",color="black"))+
  ylab("Rain (Inches)")

a<-ggplot(E5_all, aes(x=Date))+ 
  geom_line(aes(y=ft..below.ground.),linewidth=1)+
  theme_bw()+ scale_y_reverse()+ylab("Depth to Water (Feet)")+
  theme(axis.title = element_text(size=15,face="bold", color="black") ,
        axis.text = element_text(size=17,face="bold",color="black"))



b / a + plot_layout(heights = c(1, 3))

