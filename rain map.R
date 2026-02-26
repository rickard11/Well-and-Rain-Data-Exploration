E5_new<-read.csv("Data/GNSS/Escondido 5 20231001-20260225 Corrected.csv")
E5_new$Date<-as.Date(E5_new$Date.and.Time,format="%m/%d/%Y")
E5_new_daily<-aggregate(Corrected.ft.below.ground~Date,E5_new,FUN=mean)

E5_rain<-read.csv("Data/GNSS/Escondido 5 20231001-20260225 Corrected-daily.csv")
E5_rain<-E5_rain[,c(1,6)]
E5_rain$Date<-as.Date(E5_rain$Date,format="%m/%d/%Y")

E5_all<-merge(E5_rain,E5_new_daily,by="Date")
tail(E5_all)
head(E5_all)
####Or 2 seperate plots 

a<-ggplot(E5_all, aes(x=Date))+ 
  geom_line(aes(y=Corrected.ft.below.ground),linewidth=1)+
  theme_bw()+ scale_y_reverse()+ylab("Depth to Water (Feet)")

b<-ggplot(E5_all, aes(x=Date))+
  geom_col( aes(y=Rain_in),color="cornflowerblue") +
  theme_bw()+ggtitle("Escondido 5 Depth to Water and Daily Rainfall")+
  scale_y_reverse()+theme(axis.title.x = element_blank(),
                          axis.text.x = element_blank())+
  ylab("Rain (Inches)")

b / a + plot_layout(heights = c(1, 3))

ggsave("figures/Escondido_5_Timeseries_2.eps")
#