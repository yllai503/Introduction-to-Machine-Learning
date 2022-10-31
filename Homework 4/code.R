flight_original <-read.csv("C:/Users/User/Desktop/all.csv",header = T,sep = ",")
flight_original = flight_original[,-1]
flight_original = flight_original[,-25]
flight_original <- flight_original[!grepl("1",flight_original$CANCELLED),]
colSums(is.na(flight_original))
una_chen = which(is.na(flight_original$DEP_DELAY))
unaunachen <-flight_original[-una_chen,]
una_chen_version_2 = which(is.na(unaunachen$ARR_DELAY))
flight <-unaunachen[-una_chen_version_2,]
attach(flight)

write.csv(flight,"C:/Users/User/Desktop/flight.csv")
library(data.table)
fread("C:/Users/User/Desktop/flight.csv")
flight_table <- fread("C:/Users/User/Desktop/flight.csv")
flight_table[OP_CARRIER=="9E"]
result_carrier_dep = flight_table[DEP_DELAY>0,.(.N,Depart_delay_time=mean(DEP_DELAY)),by=OP_CARRIER]
result_carrier_arr = flight_table[ARR_DELAY>0,.(.N,Arrival_delay_time=mean(ARR_DELAY)),by=OP_CARRIER]
abc=merge(result_carrier_dep,result_carrier_arr,by=result_carrier_dep$OP_CARRIER)
library(ggplot2)
ggplot()+geom_bar(data=result_carrier_dep,aes(x=reorder(OP_CARRIER,-Depart_delay_time),y=Depart_delay_time,color="yellow"),stat="identity"
                  ,xlab="Company",ylab="Average Depart Delayed Time")
ggplot(result_carrier_dep,aes(x=reorder(OP_CARRIER,-Depart_delay_time),y=Depart_delay_time))+geom_bar(stat="identity",fill="#FFCC33")+xlab("Company")+ylab("Average Departure Delayed time")
ggplot(result_carrier,aes(x=reorder(OP_CARRIER,-Arrival_delay_time),y=Arrival_delay_time))+
  geom_bar(stat="identity",width = 0.8,position = "dodge",fill="skyblue")+
  geom_text()
  xlab("Company")+
  ylab("Average Arrival Delayed time")

result_time = flight_table[,.(delaytime=ARR_DELAY-DEP_DELAY,taxitime=TAXI_IN+TAXI_OUT,Carrier=OP_CARRIER)]
ggplot(data=result_time,aes(x=delaytime,y=taxitime),
      xlab="Delayed Time",ylab="Total taxi time")+geom_point()
result= result_time[c(1:10000),]
ggplot(data=result,aes(x=delaytime,y=taxitime,colour=Carrier))+
  geom_point()+
  geom_smooth()+
  scale_x_continuous("Delayed Time")+
  scale_y_continuous("Total taxi time")+
  scale_colour_discrete("Company")
time = flight_table[,.(delaytime=ARR_DELAY_NEW,dist=DISTANCE,Carrier=OP_CARRIER)]

ggplot(data=time,aes(x=delaytime,y=dist))+
  geom_point(colour="deepskyblue")+
  geom_smooth()+
  scale_x_continuous("Delayed Time")+
  scale_y_continuous("Distance")

ggplot(data=flight_table,aes(DEP_DELAY))+
  geom_histogram(bins=50)
qqnorm(flight_table$DEP_DELAY)
qqline(flight_table$DEP_DELAY)

#畫delay時間跟距離的關係圖
cutdist=cut(flight_table$DISTANCE,seq(from=0,to=5000,by=500),c("0~500","500~1000","1000~1500","1500~2000",
                                                       "2000~2500","2500~3000","3000~3500",
                                                       "3500~4000","4000~4500","4500~5000"))
flight_table$CATO_DISTANCE=cutdist
mean_time_of_dist_dep=flight_table[DEP_DELAY_NEW>0,.(meantimeofdist_dep=mean(DEP_DELAY_NEW)),by=CATO_DISTANCE]
mean_time_of_dist_arr=flight_table[ARR_DELAY_NEW>0,.(meantimeofdist_arr=mean(ARR_DELAY_NEW)),by=CATO_DISTANCE]
barplot(mean_time_of_dist_dep$meantimeofdist_dep,
        xlab="Category of Distance()")
ggplot()+
  geom_bar(data=mean_time_of_dist_dep,aes(x=CATO_DISTANCE, y=meantimeofdist_dep),stat="identity"
           ,width = 0.7)+
  theme_light()+
  scale_fill_brewer(palette = "Greens")+
  geom_text(
    aes(label = y, y = y + 0.05),
    position = position_dodge(0.9),
    vjust = 0)

#敘述統計
test=flight_table[c(1:10000)]
test2=test[DEP_DELAY>0,.(DELAY=DEP_DELAY)]
depstat=flight_table[DEP_DELAY>0,.(y=DEP_DELAY)]

#DEP_DELAY分析
#Box Plot
ggplot()+
  theme_light()+
  geom_boxplot(data=depstat,aes(x=1,y=y),color="gold")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  ggtitle("Departure Delay Time")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())
  
#Normal QQ-Plot & Normal Test
ggplot()+
  theme_light()+
  geom_qq(data=flight_table,aes(sample=DEP_DELAY),color="gold")+
  geom_qq_line(data=flight_table,aes(sample=DEP_DELAY),color="darkorange",size= 0.8)+
  ggtitle("Normal QQ-Plot")

library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
ggqqplot(test$DEP_DELAY)


shapiro.test(flight_table$DEP_DELAY) #saplesize must be between 3 and 5000
install.packages("nortest")
library(nortest)
ad.test(flight_table$DEP_DELAY) #Anderson-Darling 檢定
cvm.test(flight_table$DEP_DELAY) #Cramer-von Mises 檢定
lillie.test(flight_table$DEP_DELAY) #Lilliefors 檢定
pearson.test(flight_table$DEP_DELAY) #Pearson Chi-Squared 檢定
sf.test(flight_table$DEP_DELAY) #Shapiro-Francia 檢定 #saplesize must be between 5 and 5000

#histogram
ggplot()+
  theme_light()+
  geom_histogram(data=flight_table,aes(x=DEP_DELAY),fill="gold",binwidth = 15)+
  scale_x_continuous("Departure Delay Time")+
  scale_y_continuous(breaks=c(0,1000000,2000000,3000000,4000000),
                     labels=c("0","1000000","2000000","3000000","4000000"))

  
#ARR_DELAY分析
arrstat=flight_table[ARR_DELAY>0,.(y=ARR_DELAY)]
#Box Plot
ggplot()+
  theme_light()+
  geom_boxplot(data=arrstat,aes(x=1,y=y),color="cadetblue1")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  ggtitle("Arrival Delay Time")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

#Normal QQ-Plot & Normal Test

gogo=flight_table[c(500000:1000000),]
ggplot()+
  theme_light()+
  geom_qq(data=flight_table,aes(sample=ARR_DELAY),color="cadetblue1")+
  geom_qq_line(data=flight_table,aes(sample=ARR_DELAY),color="deepskyblue3",size= 0.8)+
  ggtitle("Normal QQ-Plot")

shapiro.test(flight_table$ARR_DELAY) #saplesize must be between 3 and 5000
install.packages("nortest")
library(nortest)
ad.test(flight_table$ARR_DELAY) #Anderson-Darling 檢定
cvm.test(flight_table$ARR_DELAY) #Cramer-von Mises 檢定
lillie.test(flight_table$ARR_DELAY) #Lilliefors 檢定
pearson.test(flight_table$ARR_DELAY) #Pearson Chi-Squared 檢定
sf.test(flight_table$ARR_DELAY) #Shapiro-Francia 檢定 #saplesize must be between 5 and 5000

#histogram
ggplot()+
  theme_light()+
  geom_histogram(data=flight_table,aes(x=ARR_DELAY),fill="darkolivegreen",binwidth = 15)+
  scale_x_continuous("Arrival Delay Time")+
  scale_y_continuous(breaks=c(0,500000,1000000,1500000,2000000,2500000),
                     labels=c("0","500000","1000000","1500000","2000000","2500000"))


#分析TAXI_OUT TAXI_IN AIR_TIME DISTANCE
#Box Plot 不合適！
library(ggplot2)
ggplot()+
  theme_light()+
  geom_boxplot(data=flight_table,aes(x=1,y=TAXI_OUT),color="darkorchid")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  ggtitle("Taxi out Time")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

ggplot()+
  theme_light()+
  geom_boxplot(data=flight_table,aes(x=1,y=TAXI_IN),color="chartreuse")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  ggtitle("Taxi In Time")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

ggplot()+
  theme_light()+
  geom_boxplot(data=flight_table,aes(x=1,y=AIR_TIME),color="brown1")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  ggtitle("Air Time")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

ggplot()+
  theme_light()+
  geom_boxplot(data=flight_table,aes(x=1,y=DISTANCE),color="darksalmon")+
  scale_x_continuous("")+
  scale_y_continuous("")+
  ggtitle("Distance(miles)")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

#Histogram
library(ggplot2)
install.packages("gcookbook")
library(gcookbook)
ggplot()+
  theme_light()+
  geom_histogram(data=flight_table,aes(x=TAXI_OUT),fill="chocolate1",binwidth = 3)+
  scale_x_continuous("Taxi Out Time")

ggplot()+
  theme_light()+
  geom_histogram(data=flight_table,aes(x=TAXI_IN),fill="chocolate3",binwidth = 3)+
  scale_x_continuous("Taxi In Time")
  
ggplot()+
  theme_light()+
  geom_histogram(data=flight_table,aes(x=AIR_TIME),fill="lightpink",binwidth = 10)+
  scale_x_continuous("Air Time")+
  scale_y_continuous(breaks=c(0,200000,400000,600000),
                     labels=c("0","200000","400000","600000"))

ggplot()+
  theme_light()+
  geom_histogram(data=flight_table,aes(x=DISTANCE),fill="lightpink3",binwidth = 30)+
  scale_x_continuous("Distance(miles)")+
  scale_y_continuous(breaks=c(0,50000,100000,150000,200000,250000))


library(nortest)
ad.test(flight_table$TAXI_OUT) #Anderson-Darling 檢定
ad.test(flight_table$TAXI_IN)
ad.test(flight_table$AIR_TIME)
ad.test(flight_table$DISTANCE)



install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
install.packages("dplyr")
library(dplyr)
library(magrittr)
#從每家航空公司中抽取1%的樣本
library(data.table)
fread("C:/Users/User/Desktop/sample.csv")
flight_sample <- fread("C:/Users/User/Desktop/sample.csv")
qqnorm(flight_sample$DEP_DELAY)
qqline(flight_sample$DEP_DELAY)
qqnorm(flight_sample$ARR_DELAY)
qqline(flight_sample$ARR_DELAY)
flight_sample$FL_DATE <- as.numeric(flight_sample$FL_DATE)
flight_sample$OP_CARRIER <- as.numeric(flight_sample$OP_CARRIER)
flight_sample$ORIGIN <- as.numeric(flight_sample$ORIGIN)
flight_sample$ORIGIN_CITY_NAME <- as.numeric(flight_sample$ORIGIN_CITY_NAME)
flight_sample$ORIGIN_STATE_NM <- as.numeric(flight_sample$ORIGIN_STATE_NM)
flight_sample$DEST <- as.numeric(flight_sample$DEST)
flight_sample$DEST_CITY_NAME <- as.numeric(flight_sample$DEST_CITY_NAME)
flight_sample$DEST_STATE_NM <- as.numeric(flight_sample$DEST_STATE_NM)
library(MASS) #boxcox資料集
boxcox(DEP_DELAY~ARR_DELAY+TAXI_IN+DISTANCE+AIR_TIME+
       OP_CARRIER+OP_CARRIER_FL_NUM+ORIGIN_STATE_NM+MONTH+
       ORIGIN+ORIGIN_CITY_NAME+DEST_STATE_NM+DEST_CITY_NAME+
       DEST+FL_DATE+DAY_OF_MONTH,data=flight_sample)
