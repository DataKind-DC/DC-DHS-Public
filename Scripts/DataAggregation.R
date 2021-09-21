
### DataKindDC Homelessness Project ###
#install.packages("GGally")
#install.packages("googlesheets4")
#install.packages("NbClust")


library(googlesheets4)
library(rnoaa)
library(tidyverse)


##Load Raw Data
weatherData<-read_sheet("https://docs.google.com/spreadsheets/d/1w8WCWvveYq2HppELyw3jebhPEO9kGZNhj8GBnegZlbM/edit#gid=1585438251")
stayData<-read_sheet("https://docs.google.com/spreadsheets/d/1m4zCOrHzPWb_GsaEr9FL30VYUUgUZbRRfWU_sMzaQK8/edit#gid=872071627")

unique(stayData$name)
n_distinct(stayData$client_id)


##Some date cleaning; filling in NAs in exit date with the day date was pulled for time calculations
datePulled<-"2021-06-11"
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
  group_by(name)%>%
  summarize(Total=sum(inRange))%>%
  mutate(date=as.Date(as.POSIXct.Date(i)))

DateCount<-bind_rows(DateCount,temp)
}



TotalDateCount<-DateCount%>%
  group_by(date)%>%
  summarise(Total=sum(Total))%>%
  mutate(dayWeek=wday(date),
         month=month(date),
         day=day(date),
         year=year(date))%>%
  left_join(weatherDataClean)%>%
  mutate(FreezingAtEntry=if_else(MinTempF<=32,1,0))



ggplot(TotalDateCount,aes(x=date,y=Total))+geom_line()
ggplot(TotalDateCount,aes(x=MinTempF,y=Total))+geom_point()
ggplot(TotalDateCount,aes(x=dayWeek,group=dayWeek,y=Total))+geom_boxplot()


##write output data locally to different dir than repo clone
setwd("C:/Users/rcarder/Documents/dev/DHS")
write.csv(TotalDateCount,"InShelterPerDay.csv",row.names = FALSE)

##The output of this script has been added to the public datacorps Google Drive folder for modeling by volunteers


