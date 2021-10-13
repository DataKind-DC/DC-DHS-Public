
### DataKindDC Homelessness Project ###

library(googlesheets4)
library(tidyverse)        

#devtools::install_github("ropensci/rnoaa")
library(rnoaa)
#remotes::install_github("ropensci/rnoaa", force=TRUE)


##Key removed; get one for NOAA API to use
options(noaakey = "NOAA_KEY")


##WeatherData
stations <- ncdc_stations(extent = c(38.9, -77.25, 39, -77))
dcstations<-stations$data
station<-"GHCND:USC00186350"
GHCND:US1MDMG0042	
ncdc_stations(stationid=station)
ncdc_datasets(stationid = station)
ncdc_datatypes(datasetid = "GHCND", stationid = station)

ncdc(datasetid = "GHCND", locationid = "ZIP:20002", datatypeid = "TMAX", 
     limit = 10, startdate = "2012-01-19", enddate = "2012-02-24")

#temp<-ncdc(datasetid = "GHCND", stationid = station, datatypeid = "TMAX", id = "HPCP", startdate = "2012-01-19", enddate = "2012-02-24")$data


staysbyYear<-stayData%>%
  mutate(year=year(start))%>%
  group_by(year)%>%
  summarize(Amount=n())


minYear<-2000
maxYear<-2021
years<-seq(from=minYear,to=maxYear, by=1)
Total<-as.data.frame(seq(from=as.Date(paste(minYear,"-01-01",sep='')),to=as.Date(paste(maxYear,"-12-31",sep='')),by=1))%>%
  dplyr::rename("date"=1)
weatherVariables<-c("TMAX","TMIN","PRCP","SNOW") ##API may time out with more than 4 variables



for (i in weatherVariables) {
  assign(paste("Total",i,sep = ''),NULL)
  for (j in years) {
   dat<-ncdc(datasetid='GHCND', stationid=station, datatypeid=i, startdate = paste(j,'-01-01',sep=''), enddate = paste(j,'-12-31',sep=''), limit=500, add_units = TRUE)$data
    
   colnames(dat)[2:9] <- paste(i, colnames(dat)[2:9], sep = "_")
   
   dat<-dat%>%
      dplyr::select(1,4,9)%>%
     mutate(date=as.Date(date))
      
   assign(paste("Total",i,sep = ''),bind_rows(get(paste("Total",i,sep = '')),dat)%>%
            mutate(date=as.Date(date)))
   
  }
  
  Total<-Total%>%
    full_join(get(paste("Total",i,sep = '')),by=c("date"))
  
}



##Write to cloned repo
setwd("C:/Users/rcarder/Documents/dev/DC-DHS-Public/external data")

write.csv(Total, "weatherdata.csv", row.names = FALSE)

