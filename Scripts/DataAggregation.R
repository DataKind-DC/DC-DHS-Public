
### DataKindDC Homelessness Project ###
#install.packages("GGally")
#install.packages("googlesheets4")
#install.packages("NbClust")


library(googlesheets4)
library(rnoaa)
library(tidyverse)
library(lubridate)
library(GGally)


##Load Raw Data
weatherData<-read_sheet("https://docs.google.com/spreadsheets/d/1w8WCWvveYq2HppELyw3jebhPEO9kGZNhj8GBnegZlbM/edit#gid=1585438251")
#stayData<-read_sheet("https://docs.google.com/spreadsheets/d/1m4zCOrHzPWb_GsaEr9FL30VYUUgUZbRRfWU_sMzaQK8/edit#gid=872071627")
stayDataGender<-read_sheet("https://docs.google.com/spreadsheets/d/1LfFB4Xcv5Io0Q8SmezZohf0J01qeqT2eyq9zsUdoBVs/edit#gid=257172774")
  
stayData<-stayDataGender

  
unique(stayData$name)
n_distinct(stayData$client_id)

table<-as.data.frame(table(stayData$end))
unique(table$Freq)
##Some date cleaning; filling in NAs in exit date with the day date was pulled for time calculations
datePulled<-as.Date("2021-09-01")
stayData$end[is.na(stayData$end)]<-datePulled
stayData$end[stayData$end=='']<-datePulled
stayData$start<-as.Date(stayData$start)
stayData$end<-as.Date(stayData$end)
weatherData$date<-as.Date(weatherData$date)
weatherData$TMIN_value<-as.numeric(weatherData$TMIN_value)
weatherData$TMAX_value<-as.numeric(weatherData$TMAX_value)
weatherData$PRCP_value<-as.numeric(weatherData$PRCP_value)
weatherData$SNOW_value<-as.numeric(weatherData$SNOW_value)

weatherDataClean<-as_tibble(weatherData)%>%
  mutate(MinTempF=TMIN_value*(9/50)+32,
         MaxTempF=TMAX_value*(9/50)+32,
         SnowIn=SNOW_value/25.4,
         PrecipIn=PRCP_value/254)%>%
  dplyr::select(1,10,11,12,13)


##create date sequence
minYear<-2005
maxYear<-2021
Dates<-seq(from=as.Date(paste(minYear,"-01-01",sep='')),to=as.Date(paste(maxYear,"-12-31",sep='')),by=1)


##Create data for each day, and how many people are in each shelter
DateCount<-NULL

for (i in Dates){
  print(as.Date(as.POSIXct.Date(i)))
  
temp<-stayData%>%
  mutate(inRange=ifelse(start<=i&end>=i,1,0))%>%
  group_by(gender)%>%
  summarize(Total=sum(inRange))%>%
  mutate(date=as.Date(as.POSIXct.Date(i)))

DateCount<-bind_rows(DateCount,temp)
}



TotalDateCount<-DateCount%>%
  pivot_wider(names_from = gender,values_from = Total)%>%
  mutate(dayWeek=factor(wday(date)),
         month=factor(month(date)),
         day=factor(day(date)),
         year=factor(year(date)))%>%
  left_join(weatherDataClean)%>%
  mutate(FreezingAtEntry=if_else(MinTempF<=32,1,0))%>%
  filter(date<="2021-09-02")



ggplot(TotalDateCount,aes(x=date,y=Male))+geom_line()
ggplot(TotalDateCount,aes(x=date,y=Female))+geom_line()
ggplot(TotalDateCount,aes(x=MinTempF,y=Male))+geom_point()
ggplot(TotalDateCount,aes(x=dayWeek,group=dayWeek,y=Total))+geom_boxplot()

ggpairs(TotalDateCount[,c(2,4,9:17)])


##write output data locally to different dir than repo clone
setwd("C:/Users/rcarder/Documents/dev/DHS")
write.csv(TotalDateCount,"InShelterPerDayGender.csv",row.names = FALSE)

##The output of this script has been added to the public datacorps Google Drive folder for modeling by volunteers


