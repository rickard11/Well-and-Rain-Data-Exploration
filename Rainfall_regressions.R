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
