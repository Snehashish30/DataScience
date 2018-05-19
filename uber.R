library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)


uber <- read.csv("Uber Request Data.csv",stringsAsFactors = F)
#View(uber)
#checking for NAs
sapply(uber,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))
#Missing Driver.id,Drop.timestamp is OK as they belong to "No Cars Avaialable" or "Cancelled" category
#date format is not uniform, time format is not uniform


#separate date and time       
uber <- separate(uber,Request.timestamp, into = c("Request.Date","Request.Time"), sep = " ", remove = T)
uber <- separate(uber,Drop.timestamp, into = c("Drop.Date","Drop.Time"), sep = " ",remove = T)
#creating correct date format
uber$Request.Date <- dmy(uber$Request.Date)
uber$Drop.Date <- dmy(uber$Drop.Date)
#correct the time format by separate,insert seconds and unite
uber <- separate(uber,Request.Time, into = c("ReqHH","ReqMM","ReqSS"), sep = ":", remove = T)
uber[which(is.na(uber$ReqSS)),8]<-"00"
uber <- unite(uber,new_request_timestamp, c("ReqHH","ReqMM","ReqSS"),sep = ":",remove = F)

uber <- separate(uber,Drop.Time, into = c("DrpHH","DrpMM","DrpSS"), sep = ":", remove = T)
uber[which(is.na(uber$DrpSS)),13]<-"00"
uber <- unite(uber,new_drop_timestamp, c("DrpHH","DrpMM","DrpSS"),sep = ":",remove = F)
#correct full datetimestamp in character
uber <- unite(uber,new_request_datetimestamp, c("Request.Date","new_request_timestamp"),sep = " ",remove = F)
uber <- unite(uber,new_drop_datetimestamp, c("Drop.Date","new_drop_timestamp"),sep = " ",remove = F)
#correct full datetimestamp in datetime format
uber$request_datetimestamp <- as.POSIXct(uber$new_request_datetimestamp)
#calculating Travel time in minutes
uber$drop_datetimestamp <-NA
uber[which(is.na(uber$Drop.Date)==F),]$drop_datetimestamp <- as.POSIXct(uber[which(is.na(uber$Drop.Date)==F),]$new_drop_datetimestamp)
uber$Travel.Time <- ifelse(uber$Status == "Trip Completed",(uber$drop_datetimestamp - unclass(uber$request_datetimestamp))/60,NA)


#remove unnecessary columns

#write.csv(uber,"uber1.csv")



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
uber$ReqHH <- as.numeric(uber$ReqHH)
#Do binning
####################################################
#Late Night :		12am - 4:59 am
#Early Morning: 	 5am - 7:59 am
#Late Morning:	 8am - 11:59 am
#Afternoon		 12pm - 4: 59pm
#Early Evening		 5pm - 6:59 pm
#Late Evening		 7pm - 11:59 pm
####################################################
uber$Req_Time_Bin <- ifelse(uber$ReqHH<5,"Late Night",ifelse(uber$ReqHH<8,"Early Morning",ifelse(uber$ReqHH<12,"Late Morning",ifelse(uber$ReqHH<17,"Afternoon",ifelse(uber$ReqHH<19,"Early Evening","Late Evening")))))
#filter the pickup and order the dataset
uber_airport <- uber%>%filter(Pickup.point == "Airport")%>%arrange((request_datetimestamp))
uber_city <- uber%>%filter(Pickup.point == "City")%>%arrange((request_datetimestamp))
#uber$Wait.Time in minutes
uber_airport$Wait.Time <- uber_airport$request_datetimestamp - lag(uber_airport$request_datetimestamp, n = 1, default = NA )
uber_airport$Wait.Time <- uber_airport$Wait.Time/60

uber_city$Wait.Time <- uber_city$request_datetimestamp - lag(uber_city$request_datetimestamp, n = 1, default = NA )
uber_city$Wait.Time <- uber_city$Wait.Time/60


#analysis

#Situtation at Airport

uber_airport%>%group_by(Request.Date,Req_Time_Bin)%>%summarize(avg_travel_time = mean(Travel.Time,na.rm = T),avg_interval = mean(Wait.Time,na.rm = T))
# we can see the average travel time across the days on various time slots is close to 50 min.
# we can observe the average wait time across the days on various time slots is not more than 6 min.

#write.csv(uber,"uber.csv")


# first glance at the situation to get an overall feel
ggplot(uber_airport, aes(x = ReqHH))+geom_bar(aes(fill = Status),position = "dodge")
ggplot(uber_airport, aes(x = factor(Req_Time_Bin, levels= c("Late Night","Early Morning","Late Morning","Afternoon","Early Evening","Late Evening"))))+geom_bar(aes(fill = Status),position = "dodge")+xlab("Timeslots")+ggtitle("Airport")
###############################
#bar plot is chosen to show the counts of the various trip status in 
#different timeslots
###############################
#aggregating on various categories to visualise the spread 
grp1 <-uber_airport%>%group_by(Request.Date,Req_Time_Bin)%>%summarize(total_requests = length(Request.id))
grp1
#plot showing distribution of number of requests on 6 days at various time slots
ggplot(grp1,aes(x = Request.Date, y = total_requests))+geom_line(aes(col= Req_Time_Bin))+geom_point(aes(col= Req_Time_Bin))+ggtitle("Total requests at Airport")
###############################
#line graph is chosen to show the trend of the requests in
#different timeslots on different days. color marks is used
# to show the diffrent timeslots
###############################

# The distribution has some variance and hence going for mean


#plotting the distribution of trips completed on 6 days at various hours for various trip status
grp <-uber_airport%>%group_by(Request.Date,Req_Time_Bin,Status)%>%summarize(trip_type_count = length(Status))
grp1 <- grp%>%filter(Status == "Trip Completed")
ggplot(grp1,aes(x = Request.Date, y = trip_type_count))+geom_line(aes(col= Req_Time_Bin))+geom_point(aes(col= Req_Time_Bin))+ggtitle("Trips Completed from Airport")
grp1 <- grp%>%filter(Status == "Cancelled")
ggplot(grp1,aes(x = Request.Date, y = trip_type_count))+geom_line(aes(col= Req_Time_Bin))+geom_point(aes(col= Req_Time_Bin))
#ditribution shows huge variance across the days
grp
grp2 <- grp%>%group_by(Req_Time_Bin,Status)%>%summarize(trip_type_avg = floor(mean(trip_type_count)))%>%setNames(c("Req_Time_Bin","Status","count"))
grp2 
grp3 <- grp2%>%group_by(Req_Time_Bin,"Total_Requests")%>%summarize(tot_requests_made = sum(count))%>%setNames(c("Req_Time_Bin","Status","count"))
grp3
grp2 <- rbind(grp2,grp3)
grp2
ggplot(grp2,aes(x = factor(Req_Time_Bin, levels= c("Late Night","Early Morning","Late Morning","Afternoon","Early Evening","Late Evening")), y = count))+geom_bar(stat = "identity",aes(fill = Status), position = "dodge")+ggtitle("Request/Trip status Counts from Airport")+xlab("Timeslots")
###############################
#bar plot is chosen to show the counts of the various trip status in 
#different timeslots
###############################
grp2 <- grp2%>%arrange(desc(Req_Time_Bin),desc(count))
grp2
grp2<- dcast(grp2,Req_Time_Bin~Status)
grp2
#calculating cancellation and gap percentage
grp2$cancellation_percentage <- 100*(grp2$Cancelled/grp2$Total_Requests)
grp2$gap_percentage <- 100*(grp2$Total_Requests - grp2$`Trip Completed`)/grp2$Total_Requests
grp2

##################################################
#It is clear that at Airport, the demand is at its peak at Late Evening(7pm - 12am) and despite of 
#low cancellation 5% , the gap is huge 73%
#There has been low organic supply from City in the Early Evening slots
#Details in the ppt.
##################################################


#Situtation at City

uber_city%>%group_by(Request.Date,Req_Time_Bin)%>%summarize(avg_travel_time = mean(Travel.Time,na.rm = T),avg_interval = mean(Wait.Time,na.rm = T))
# we can see the average travel time across the days on various time slots is close to 52 min.
# we can observe the average wait time across the days on various time slots is not more than 2 min.
#write.csv(uber,"uber.csv")


# first glance at the situation to get an overall feel
ggplot(uber_city, aes(x = ReqHH))+geom_bar(aes(fill = Status),position = "dodge")
ggplot(uber_city, aes(x = factor(Req_Time_Bin, levels= c("Late Night","Early Morning","Late Morning","Afternoon","Early Evening","Late Evening"))))+geom_bar(aes(fill = Status),position = "dodge")+xlab("Timeslots")+ggtitle("City")
###############################
#bar plot is chosen to show the counts of the various trip status in 
#different timeslots
###############################
grp1 <-uber_city%>%group_by(Request.Date,Req_Time_Bin)%>%summarize(total_requests = length(Request.id))
grp1
#plot showing distribution of number of requests on 6 days at various time slots
ggplot(grp1,aes(x = Request.Date, y = total_requests))+geom_line(aes(col= Req_Time_Bin))+geom_point(aes(col= Req_Time_Bin))+ggtitle("Total Requests in the City")
# The distribution has some variance and hence going for mean


#plotting the distribution of trips completed on 6 days at various hours
grp <-uber_city%>%group_by(Request.Date,Req_Time_Bin,Status)%>%summarize(trip_type_count = length(Status))
grp1 <- grp%>%filter(Status == "Trip Completed")
ggplot(grp1,aes(x = Request.Date, y = trip_type_count))+geom_line(aes(col= Req_Time_Bin))+geom_point(aes(col= Req_Time_Bin))+ggtitle("Trips Completed from City")
grp1 <- grp%>%filter(Status == "Cancelled")
ggplot(grp1,aes(x = Request.Date, y = trip_type_count))+geom_line(aes(col= Req_Time_Bin))+geom_point(aes(col= Req_Time_Bin))
#ditribution shows huge variance across the days
grp
grp2 <- grp%>%group_by(Req_Time_Bin,Status)%>%summarize(trip_type_avg = floor(mean(trip_type_count)))%>%setNames(c("Req_Time_Bin","Status","count"))
grp2 
grp3 <- grp2%>%group_by(Req_Time_Bin,"Total_Requests")%>%summarize(tot_requests_made = sum(count))%>%setNames(c("Req_Time_Bin","Status","count"))
grp3
grp2 <- rbind(grp2,grp3)
grp2
ggplot(grp2,aes(x = factor(Req_Time_Bin, levels= c("Late Night","Early Morning","Late Morning","Afternoon","Early Evening","Late Evening")), y = count))+geom_bar(stat = "identity",aes(fill = factor(Status)), position = "dodge")+ggtitle("Request/Trip status Counts from City")+xlab("Timeslots")
###############################
#bar plot is chosen to show the counts of the various trip status in 
#different timeslots
###############################
grp2 <- grp2%>%arrange(desc(Req_Time_Bin),desc(count))
grp2
grp2<- dcast(grp2,Req_Time_Bin~Status)
grp2$cancellation_percentage <- 100*(grp2$Cancelled/grp2$Total_Requests)
grp2$gap_percentage <- 100*(grp2$Total_Requests - grp2$`Trip Completed`)/grp2$Total_Requests
grp2

##################################################
#It is clear that in the City, the demand rises at Morning(6am - 12pm) and
#cancellation rate is also high 44.4% which results in huge gap of 68.67
#There has been a very low organic supply from Airport in the prev time slot
#Details in the ppt.
##################################################

