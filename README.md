# Bike-sharing-data-analysis

--------------------------------------------------------------------
### STEP 1: CREATING WORK ENVIRONMENT
--------------------------------------------------------------------

#### Installing packages
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)

#### Setting work directory

getwd()
setwd("/Users/Irina/Documents/DATA ANALYST/COURSE 8 - Capstone Project/My_Project/CSV_files_for_R/CSV_files")
knitr::opts_knit$set(root.dir = "/Users/Irina/Documents/DATA ANALYST/COURSE 8 - Capstone Project/My_Project/CSV_files_for_R/CSV_files")

------------------------------------------------------------------
### STEP 2: COLLECTING DATA
-------------------------------------------------------------------
#### Reading csv files
october<-read.csv("2021_10_cyclistic_tripdata.csv", TRUE, ",")
november<-read.csv("2021_11_cyclistic_tripdata.csv", TRUE, ",")
december<-read.csv("2021_12_cyclistic_tripdata.csv", TRUE, ",")
january<-read.csv("2022_01_cyclistic_tripdata.csv", TRUE, ",")
february<-read.csv("2022_02_cyclistic_tripdata.csv", TRUE, ",")
march<-read.csv("2022_03_cyclistic_tripdata.csv", TRUE, ",")
april<-read.csv("2022_04_cyclistic_tripdata.csv", TRUE, ",")
may<-read.csv("2022_05_cyclistic_tripdata.csv", TRUE, ",")
june<-read.csv("2022_06_cyclistic_tripdata.csv", TRUE, ",")
july<-read.csv("2022_07_cyclistic_tripdata.csv", TRUE, ",")
august<-read.csv("2022_08_cyclistic_tripdata.csv", TRUE, ",")
september<-read.csv("2022_09_cyclistic_tripdata.csv", TRUE, ",")

#### Combinig 12 datasets into 1. 

combined_data<-rbind(october,november,december,january,february,march,april,may,june,july,august,september)

------------------------------------------------------------------------------------
### STEP 3: CLEANING UP AND PREPARING DATA FOR ANALYSIS
------------------------------------------------------------------------------------
#### Inspecting the new table that has been created
head(combined_data)
str(combined_data)
summary(combined_data)

#### Convert character to date/time format in "started_at" and "ended_at" columns. Also calculating a new column for trip length in minutes, changing its format from chr to numeric, and deleting "started_at" and "ended_at" columns

combined_data<-combined_data %>%
  mutate(start=mdy_hm(started_at)) %>%
  mutate(end=mdy_hm(ended_at)) %>%
  mutate(trip_length_min=(end-start)/60) %>%
  mutate(trip_length_min= as.numeric(trip_length_min)) %>%
  select(-started_at, -ended_at)

#### Adding columns that list the date, month, day, and year of each ride. This will allow us to aggregate ride data for each month, day, or year.

combined_data$date<-as.Date(combined_data$start)
combined_data$month<-format(as.Date(combined_data$date), "%m")
combined_data$day<-format(as.Date(combined_data$date), "%d")
combined_data$year<-format(as.Date(combined_data$date), "%Y")
combined_data$day_of_week<-format(as.Date(combined_data$date), "%A")

------------------------------------------------------------------------------------
### STEP 4: CONDUCTING DESCRIPTIVE ANALYSIS ON DURATION OF RIDES
-----------------------------------------------------------------------------------
#### Descriptive analysis on ride length for members and casual riders combined (all figures in minutes)

*Notes*: We know from the number of observations that the total number of rides is 5763638. Now we know the total trip length in minutes - 114260086, average trip length - 19.8, and maximum trip length - 40705

sum(combined_data$trip_length_min)
mean(combined_data$trip_length_min)
max(combined_data$trip_length_min)

#### Comparing members and casual riders: grouping by "mem_casual” and summarizing number of trips, trip hours, avg trip length, and max trip length

combined_data %>%
  group_by(member_casual) %>%
 summarise(trips_hours=sum(trip_length_min)/60,avr_trip_min=mean(trip_length_min), max_trip_length=max(trip_length_min))

*Note*: as we know from the previous calculation, the average duration of trips is 20 mins. Now, members’ average trip is 13 min, and casual users ride 2.7x longer on average – 30 mins.

#### Calculating number of trips for members vs casual riders

combined_data %>%
  count(member_casual)

*Note*: Members take more trips than casual riders. Casual riders account for 41% of the total number of rides.

#### Calculating the average trip time by each day for members vs casual riders

aggregate(combined_data$trip_length_min~combined_data$member_casual+combined_data$day_of_week, FUN=mean)

#### Fixing the order of the days of the week

combined_data$day_of_week<-ordered(combined_data$day_of_week,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#### Running the average trip time by days of the week for members vs casual riders again

aggregate(combined_data$trip_length_min~combined_data$member_casual+combined_data$day_of_week, FUN=mean) %>%

  print(14)

#### Analyzing ridership data by rider type and weekday

combined_data %>%
  mutate(weekday=wday(start,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,weekday)

---------------------------------------------
### STEP 5: VISUALIZING DATA
---------------------------------------------

#### Visualizing the number of rides by rider and weekday

combined_data %>
  mutate(weekday=wday(start,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position ="dodge")+
  labs(title = "Number of Rides by Rider Type and Weekday")

*Notes*: For members the highest number of rides falls on Tuesday, Wednesday, and Thursday. Casual riders, on the other hand, make more rides on weekends, with the lowest numbers on Monday, Tuesday, and Wednesday.

#### Visualizing average duration of rides by rider and weekday

combined_data %>%
  mutate(weekday=wday(start,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=average_duration_min, fill=member_casual))+
  geom_col(position ="dodge")+
  labs(title = "Average Duration of Rides by Rider Type and Weekday")

*Note*: The average duration of rides for casual riders is the highest on Sundays, Saturdays and Mondays.  The average duration of rides for members is very consistent all through the week with a small uptick on Saturday and Sunday.

#### Analyzing number of rides by rider type and month

combined_data %>%
  mutate(month=month(start,label=TRUE)) %>%
  group_by(member_casual,month) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,month)

#### Visualizing the number of rides by rider and month (bar chart)

combined_data %>%
  mutate(month=month(start,label=TRUE)) %>%
  group_by(member_casual, month) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,month) %>%
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual))+
  geom_col(position ="dodge")+
  labs(title = "Number of Rides by Rider Type and Month")

#### Visualizing the number of rides by rider type and month (line chart)

combined_data %>%
  mutate(month=month(start,label=TRUE)) %>%
  group_by(member_casual, month) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,month) %>%
  ggplot(aes(x=month, y=number_of_rides, group=member_casual, color=member_casual))+
  geom_line(size=1)+
  labs(title = "Number of Rides by Rider Type and Month")

*Note*: Both members and casual riders follow a similar pattern: lowest numbers in January and February and a steady climb beginning in March. However, for members, the numbers reach their peak in August; and for casual riders – in July.

#### Visualizing average duration of rides by rider type and month

combined_data %>%
  mutate(month=month(start,label=TRUE)) %>%
  group_by(member_casual, month) %>%
 summarize(number_of_rides=n(),average_duration_min=mean(trip_length_min)) %>%
  arrange(member_casual,month) %>%
  ggplot(aes(x=month, y=average_duration_min,fill=member_casual))+
  geom_col(position="dodge")+
  labs(title = "Average Duration of Rides by Rider Type and Month")

*Note*: We notice right away that the average duration of casual riders' trips is significantly higher than that of members, with the highest numbers in March and June and the lowest in November. For members, the highest average duration falls on May, June, July, and the lowest - November, December.

------------------------------------------------------------------------------------
### STEP 6: CONDUCTING DESCRIPTIVE ANALYSIS ON DISTANCE
-----------------------------------------------------------------------------------

#### Creating vectors

x<-combined_data %>%
  select(c(start_lng, start_lat))
y<-combined_data %>%
  select(c(end_lng, end_lat))

#### Adding a distance column

combined_data<-combined_data %>%
  mutate(distance=distHaversine(x,y,r=3961))

*Note*: right away I saw some observations with 0 for the distance variable, even though the trip duration (trip_length_min) for the same observations wasn't 0. I decided to see how many observations in the dataset have 0 in the distance column.

#### Counting rows with "0" for distance

 sum(combined_data$distance==0,na.rm=TRUE)

*Note*: 4091058 rows don't show any distance. Conclusion: I decided to exclude the distance data from the analysis. If I worked on a team, I would find out more about the tracking system for miles that Cyclistic uses for their bikes (if any).

------------------------------------------------------------------------------------
### STEP 7: CONDUCT DESCRIPTIVE ANALYSIS OF BIKE PREFERENCES
------------------------------------------------------------------------------------

combined_data %>%
  group_by(member_casual, rideable_type) %>%
  summarize(count=n()) %>%
  arrange(member_casual, rideable_type)

#### Visualizing bike preferences for casual riders vs members

combined_data %>%
  group_by(member_casual, rideable_type) %>%
  summarize(count=n()) %>%
  arrange(rideable_type, member_casual) %>%
  ggplot(aes(rideable_type, count, fill=member_casual))+
  geom_col()+
  labs(title="Bike Preferences by Rider Type")

*Note*: There is a difference in bike preferences between members and casual riders. Unlike members whose number 1 preference is classic bikes followed by electric bikes, more casual riders prefer electric bikes over classic bikes, and they are the only ones that use docked bikes.

------------------------------------------------------------------------------------
### STEP 8: ANALYSIS OF TOP 5 START AND END STATIONS
------------------------------------------------------------------------------------

#### Top 5 start stations for members

*Note*: Kingsbury St & Kinzie St, Clark St & Elm St, Wells St & Concord Ln, Clinton St & Washington Blvd, Ellis Ave & 60th St

combined_data %>%
  filter(member_casual=="member") %>%
  group_by(start_station_name) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  print(n=6)

#### Top 5 end stations for members

*Note*: Kingsbury St & Kinzie St, Clark St & Elm St, Wells St & Concord Ln, University Ave & 57th St, Clinton St & Washington Blvd                       
combined_data %>%
  filter(member_casual=="member") %>%
  group_by(end_station_name) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  print(n=6)

#### Top 5 start stations for casual riders

*Note*: Streeter Dr & Grand Ave, DuSable Lake Shore Dr & Monroe St, Millennium Park, Michigan Ave & Oak St, DuSable Lake Shore Dr & North Blvd

combined_data %>%
  filter(member_casual=="casual") %>%
  group_by(start_station_name) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  print(n=6)

#### Top 5 end stations for casual riders

*Note*: Streeter Dr & Grand Ave, DuSable Lake Shore Dr & Monroe St, Millennium Park, Michigan Ave & Oak St, DuSable Lake Shore Dr & North Blvd

combined_data %>%
  filter(member_casual=="casual") %>%
  group_by(end_station_name) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  print(n=6)

*Note*: Top 5 start and end stations for casual riders are all popular touristic location with an easy access to places like Adler Planetarium, Navy Pier, Field Museum, Shedd Aquarium, a variety of beaches, etc
