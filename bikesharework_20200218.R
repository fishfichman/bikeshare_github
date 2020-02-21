#BikeShareWork by Rachel Fichman 2/17/20
library(tidyverse)

#load in raw data, match column names, and clean up User.Type so that blanks are categorized as "Unknown"

ny <- read.csv('new_york_city.csv', stringsAsFactors = FALSE)  %>%
  mutate(User.Type = case_when(User.Type == ""~"Unknown", User.Type == NA~"Unknown", TRUE ~ User.Type)) %>%
  mutate(City = 'New York')
chi <- read.csv('chicago.csv', stringsAsFactors = FALSE) %>%
  mutate(User.Type = case_when(User.Type == ""~"Unknown", User.Type == NA~"Unknown", TRUE ~ User.Type)) %>%
  mutate(City = 'Chicago')
wash <- read.csv('washington.csv', stringsAsFactors = FALSE) %>%
  mutate(User.Type = case_when(User.Type == ""~"Unknown", User.Type == NA~"Unknown", TRUE ~ User.Type)) %>%
  mutate(City = "Washington") %>%
  mutate(Gender = NA) %>%
  mutate(Birth.Year = NA)

#bind all together into one df
allcities <- rbind(ny, chi, wash)

### Question 1

#What is the sum of Trip Durations for each city? How do they compare to each other?

#Trip.Duration refers to how long the bike was rented, no the amount of time spent riding the bike.
#Sum of Trip Durations for each city
sumchi <- sum(chi$Trip.Duration, na.rm = TRUE)
sumny <- sum(ny$Trip.Duration, na.rm = TRUE)
sumwash <- sum(wash$Trip.Duration, na.rm = TRUE)

#the total trip durations for each city vary drastically. Chicago has a total of 8,087,801 seconds, New York has 49,490,073 seconds, and Washington has 1,098,835,545

#Visualize Sum Comparisons
sumcompare <- ggplot(data = subset(allcities,!is.na(Trip.Duration)), aes(x = City, y = Trip.Duration)) +
  geom_histogram(stat = 'sum') +
  theme(legend.position = "none") +
  ggtitle('Sum Trip Duration in Seconds Per City') +
  labs(y='Sum Trip Duration')
sumcompare

#the number of seconds logged in Washington is MUCH higher than New York or Chicago and Chicago has far fewer seconds logged than the others

#**Overall, the total trip durations for each city vary drastically. Chicago has a total of 8,087,801 seconds, New York has 49,490,073 seconds, and Washington has 1,098,835,545. As seen in the histogram named "sumcompare", the number of seconds logged in Washington is MUCH higher than New York or Chicago and Chicago has far fewer seconds logged than the others. **

### Question 2

#How do these cities' mean, and median trip durations compare? What is the most common travel time (in minutes) for each city?

#Summary of Each City - shows median and mean, but not mode (most common)

summary(ny$Trip.Duration)
summary(wash$Trip.Duration)
summary(chi$Trip.Duration)

# Looks like the median time for all cities is between 10-12 minutes (specifically 610, 707 and 670 seconds) but the mean for Washington is much higher than the others (specifically 903.6, 1234, 937.2 seconds)

#Raw Box Plot of Trip Duration
rawboxall <- ggplot(data = subset(allcities, !is.na(Trip.Duration)), aes(y = Trip.Duration, color = City)) +
  geom_boxplot() +
  ggtitle('Raw Trip Duration Summary Stats') +
  labs(y = 'Trip Duration (seconds)')  +
  theme(legend.position = "none") +
  facet_wrap(~City)
rawboxall

#Clearly huge outliers (probably because it's rental time, not biking time so individuals could have the meter runnings for very long periods of time)

#Zoom-In on Median
zoomboxall <- ggplot(data = subset(allcities, !is.na(Trip.Duration)), aes(y = Trip.Duration, color = City)) +
  geom_boxplot() +
  ggtitle('Trip Duration Summary Stats for All Cities') +
  scale_y_continuous(limits = c(0,1500), breaks = seq(0, 1500, 60)) +
  labs(y = 'Trip Duration (seconds)')  +
  theme(legend.position = "none") +
  facet_wrap(~City)
zoomboxall

#Visualize most common ride length (by minute) in each city
histall <- ggplot(data = subset(allcities, !is.na(Trip.Duration)), aes(x = Trip.Duration, color = City)) +
  geom_histogram(binwidth = 60) +
  scale_x_continuous(limits = c(0,4000), breaks = seq(0, 4000, 500))
histall

#Zoom-in to view peaks more closely
zoomhistall <- ggplot(data = subset(allcities, !is.na(Trip.Duration)), aes(x = Trip.Duration, color = City)) +
  geom_histogram(binwidth = 60) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0, 4000, 60)) +
  scale_y_continuous(limits = c(0, 11000), breaks = seq(0, 11000, 500))
zoomhistall

#Wow! The most common bin of trip durations (binned by 1 minute) is at 360 seconds (around 6 minutes) in all cities!

#**Looks like the median time for all cities is between 10-12 minutes (specifically 610, 707 and 670 seconds) but the mean for Washington is much higher than the others (specifically 903.6, 1234, 937.2 seconds). Also, the most common trip duration rounded by 1 minute is 6 minutes for each city!**

### Question 3

#Who tend to take short rides, Subscribers and Customers?

#Again zooming in to the box plot to visualize difference between user types
zoomcomp <- ggplot(data = subset(allcities, !is.na(Trip.Duration)), aes(x = User.Type, y = Trip.Duration)) +
  geom_boxplot() +
  #clearly some very large outliers (left the meter running for a very long time in ny) so subset data shown
  scale_y_continuous(limit = c(0,1500)) +
  ggtitle('Trip Duration x User Type x City') +
  labs(x = 'User Types', y = 'Trip Duration') +
  facet_wrap(~City)
zoomcomp

#**From this boxplot comparing subscribers and customers split by city, looks like subscribers tend to take shorter rides than customers in each city.**
