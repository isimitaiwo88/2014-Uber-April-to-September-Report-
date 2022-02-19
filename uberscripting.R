install.packages("ggplot2")
install.packages("ggthemes")
install.packages("dplyr")
install.packages("lubridate")
install.packages("scales")
install.packages("tidyr")
install.packages("DT")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(scales)
library(tidyr)
library(DT)

#reading the csv
apr_data <-read.csv("./uber-raw-data-apr14.csv")
may_data <-read.csv("./uber-raw-data-may14.csv")
jun_data <-read.csv("./uber-raw-data-jun14.csv")
jul_data <-read.csv("./uber-raw-data-jul14.csv")
aug_data <-read.csv("./uber-raw-data-aug14.csv")
sep_data <-read.csv("./uber-raw-data-sep14.csv")

#combining all the data
data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)


#visualize the data
head(data_2014)

#structure
str(data_2014)

#summary statistics
summary(data_2014)

#what are your primary observation from date time, we get the time frame
#lat lon gives the data is some what taken from there usa operation
#they have 5 bases there.

#start analysis

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

#summary statistics
summary(data_2014)

#Extracting time from date time
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")

#confirm
data_2014$Time

#day and months

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)#formatting the date
data_2014$day <- format(day(data_2014$Date.Time))#day
data_2014$month <- format(month(data_2014$Date.Time, label = TRUE))#month
data_2014$year <- format(year(data_2014$Date.Time))#no meaning as we have data for only 2014
data_2014$dayofweek <- format(wday(data_2014$Date.Time, label = TRUE))#day of the week


#hour minute second

data_2014$hour <- factor(hour(hms(data_2014$Time))) #we want all of these as factor
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

head(data_2014)

#visualization

#plotting the trip by the hour of the day

hour_data <- data_2014 %>%
  group_by(hour) %>%
  summarise(Total= n()) #grouping the data based on hours

#see the hour data in table form
datatable(hour_data)

#visualize the hour data with bar chart
ggplot(hour_data, aes(hour, Total)) +
  geom_bar(stat = "identity", fill = "black", color = "blue") +
  ggtitle("Number of Trips By  the Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#we noticed that most activities happen around the 15th to 21st hour 
#aka 3pm to 9pm

#total trips per the hour of each month
month_hour_data <- data_2014 %>%
  group_by(month,hour) %>%
  summarise(Total = n())

#see in table form
datatable(month_hour_data)

#visualize the trips by the hour of the months
ggplot(month_hour_data, aes(hour, Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Hour and month") +
  scale_y_continuous(labels = comma)

#Finding from the graph September has more rides than any other month

#lets get a deep dive into September trips
sept_hour <- data_2014 %>%
  group_by(hour, month) %>%
  filter(month == "Sep") %>%
  summarise(Total = n())

ggplot(sept_hour, aes(hour, Total, fill=hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Hour for the month of September") +
  scale_y_continuous(labels = comma)
#around 6pm it goes above 70,000 rides,peaks 75,000 by 7pm 
#then by 8pm drops below 70,000 rides
#by 11 it goes below 50,000 rides and drops to it lowest 8,000 by 3am 


#lets get a deep dive into April trips
apr_hour <- data_2014 %>%
  group_by(hour, month) %>%
  filter(month == "Apr") %>%
  summarise(Total = n())

ggplot(apr_hour, aes(hour, Total, fill=hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Hour for the month of April") +
  scale_y_continuous(labels = comma)
#on the 17th hour(6pm) it hits above the 50,000 rides

#Trips grouped by day
day_data <- data_2014 %>%
  group_by(day) %>%
  summarise(Total= n())

#see in table form
datatable(day_data)

#visualize the day data with bar chart
ggplot(day_data, aes(day, Total)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  ggtitle("Total Number of Trips By  the day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
#basically uniformed distribution of trips apart from 31 
#and it is because alot of months don't have 31st

#do a month and day grouping
month_day_data <- data_2014 %>%
  group_by(month,day)%>%
  summarise(Total= n())

#see in table form
datatable(month_day_data)

ggplot(month_day_data, aes(day, Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Day and Month") +
  scale_y_continuous(labels = comma)

#finding data worth of 180 days and since we know September has the highest trip
#in the year 2014, we should output a graph to understand the day with
#the highest trips on the month of September

#September data
sept_day <- data_2014 %>%
  group_by(day, month) %>%
  filter(month == "Sep") %>%
  summarise(Total = n())

ggplot(sept_day, aes(day, Total, fill=day)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By day for the month of September") +
  scale_y_continuous(labels = comma)

#the 13th day has the highest trips


#monthly trend
month_data <- data_2014 %>%
  group_by(month)%>%
  summarise(Total = n())

datatable(month_data)

ggplot(month_data, aes(month, Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Month") +
  scale_y_continuous(labels = comma)
#no particular trend spotted
#September and August has the highest Number Trips

#month-weekday
#the Day with the highest amount of trips per month
month_weekday_data <- data_2014 %>%
  group_by(month,dayofweek) %>%
  summarise(Total= n())

#in table format
datatable(month_weekday_data)

ggplot(month_weekday_data, aes(month, Total, fill=dayofweek)) +
  geom_bar(stat = "identity") +
  ggtitle("The Day with the highest Trip for each Month") +
  scale_y_continuous(labels = comma)

#only weekday
#the day with the highest amount of trips in total
weekday_data <- data_2014 %>%
  group_by(dayofweek) %>%
  summarise(Total= n())

#in table format
datatable(weekday_data)

#visualization
ggplot(weekday_data, aes(dayofweek, Total, fill=dayofweek)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By day for the month of September") +
  scale_y_continuous(labels = comma)
#Thursday and Friday is the busiest 
#any reason why these days has more traffic

#analysis of Bases
ggplot(data_2014,aes(Base)) +
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Base")
#it seems B02512 and B02764 aren't getting much order
#hence they are not much profitable

#trips based on the bases and the months of operation
ggplot(data_2014,aes(Base, fill=month)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Base and Months")
#Events in September on base B02764 and April and May of B02617
#must be observed.

#Analysis on day of the week based on base
ggplot(data_2014,aes(Base, fill=dayofweek)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Base and Day of Week")


#analysis of hours and days
day_and_hour <- data_2014 %>%
  group_by(day,hour) %>%
  dplyr::summarise(Total= n())
datatable(day_and_hour)

ggplot(day_and_hour,aes(day,hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")
#As confirmed by previous chart 15 to 21 hr is usually the most 
#profitable hours of the day


#from summary stat
#summary(data_2014)
min_lat <- 40.5774
max_lat <- 40.9176
min_long<- -74.15
max_long<- -73.7004

ggplot(data_2014,aes(x=Lon, y=Lat)) +
  geom_point(size=1) +
  scale_x_continuous(limits = c(min_long,max_long)) +
  scale_y_continuous(limits = c(min_lat,max_lat)) +
  theme_map() +
  ggtitle("NYC(lat-lon chart) MAP based on UBER Rides during 2014(APR-SEP) By BASE")



