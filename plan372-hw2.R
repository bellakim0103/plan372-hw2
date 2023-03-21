# Do some exploratory data analysis of the restaurant_inspections data
# The restaurant dataset contains records of the most recent health inspection for food-service establishments in Wake County. 
# Each row represents the most recent inspection for a particular food-service establishment.

library(tidyverse)  # library for dealing with tabular data
library(lubridate)  # library for handling dates

data = read_csv("restaurant_inspections.csv")
View(data[1:20,])

# we have twelve columns:
#   OBJECTID – numeric identifier of a particular inspection 
#   HSISID – numeric identifier of a particular restaurant 
#   SCORE – the sanitation score of the restaurant (0-100) 
#   DATE_ - the date of the inspection 
#   DESCRIPTION – a description of any issues found at the inspection 
#   TYPE – Whether this was a normal inspection, or a re-inspection after correction issues from a 
#   previous inspection 
#   INSPECTOR – who conducted this inspection 
#   PERMITID – the Food Service Permit identifier for the facility 
#   NAME – the name of the facility 
#   RESTAURANTOPENDATE – when the restaurant first opened 
#   FACILITYTYPE – whether this is a restaurant, food truck, etc. 

#   1) Visualize the overall distribution of inspection scores using a histogram.
hist(data$SCORE)

#   2) Some restaurants have been in business much longer than others. Is there any trend in 
#   terms of how highly older vs. newer restaurants score on their inspections?

#   First, convert the values from the RESTAURANTOPENDATE column from chr to month/day/year format.
data = mutate(data, RESTAURANTOPENDATE=ymd_hms(RESTAURANTOPENDATE))

#   Next, look at the SCORES by year to look for trends over time.
data$year = year(data$RESTAURANTOPENDATE)
unique(data$year)

yearly_scores = group_by(data, year) %>%
  summarize(mean_score=mean(SCORE))

ggplot(yearly_scores, aes(x=year, y=mean_score)) +
  geom_line()

# Any missing values? 
data[!is.na(data$RESTAURANTOPENDATE),]
# Yes, there are 296 missing values for RESTAURANTOPENDATE. These NA values have been ignored in the plot.



#   3) Do the inspection scores vary by city?

#   First, recode city names so that there is only one estimated value per city.
#   a) Capitalize all city names
data$CITY = str_to_upper(data$CITY)
unique(data$CITY)

#   b) Correct any misspellings or extra characters.
data$CITY = recode(data$CITY, "FUQUAY-VARINA"="FUQUAY VARINA", "RTP"="RESEARCH TRIANGLE PARK", 
                   "HOLLY SPRING"="HOLLY SPRINGS", "MORRISVILE"="MORRISVILLE")

#   Now, follow the same steps as #2 to group inspection scores by city.
city_scores = group_by(data, CITY) %>%
  summarize(mean_score=mean(SCORE))

ggplot(city_scores, aes(x=CITY, y=mean_score, group=1)) +
  geom_point() +
  geom_line()


#   4) Similar to #3, now group the inspection scores by inspector.
inspector_scores = group_by(data, INSPECTOR) %>%
  summarize(mean_score=mean(SCORE))

ggplot(inspector_scores, aes(x=INSPECTOR, y=mean_score, group=1)) +
  geom_point() +
  geom_line()

#   5) Is it possible that some extreme results from the previous questions are due to small
#   sample sizes in a particular city, for a particular inspector, or in a particular time period?

#   Count the number of entries for each of the groups, and examine whether the extreme values have 
#   smaller number of entries. 
table(data$year, useNA = 'always')
table(data$CITY)
table(data$INSPECTOR)

#   6) Are the scores for restaurants higher than other types of facility?
#   Group inspection scores by facility type.
facility_scores = group_by(data, FACILITYTYPE) %>%
  summarize(mean_score=mean(SCORE))

#   7) Repeat steps 1-5 for restaurants only. 
restaurant_data = filter(data, FACILITYTYPE == "Restaurant")

#   Create a histogram of inspection scores. 
hist(restaurant_data$SCORE)

#   Group and plot inspection scores by year. 
restaurant_data$year = year(restaurant_data$RESTAURANTOPENDATE)
unique(restaurant_data$year)

restaurant_yearly_scores = group_by(restaurant_data, year) %>%
  summarize(mean_score=mean(SCORE))

ggplot(restaurant_yearly_scores, aes(x=year, y=mean_score)) +
  geom_line()

#   Group and plot inspection scores by city.
restaurant_city_scores = group_by(restaurant_data, CITY) %>%
  summarize(mean_score=mean(SCORE))

ggplot(restaurant_city_scores, aes(x=CITY, y=mean_score, group=1)) +
  geom_point() +
  geom_line()

#   Group and plot inspection scores by inspector.
restaurant_inspector_scores = group_by(restaurant_data, INSPECTOR) %>%
  summarize(mean_score=mean(SCORE))

ggplot(restaurant_inspector_scores, aes(x=INSPECTOR, y=mean_score, group=1)) +
  geom_point() +
  geom_line()

# Examine the sample sizes of extreme values of restaurant inspection scores 
# grouped by date, city, and inspector.
table(restaurant_data$year, useNA = 'always')
table(restaurant_data$CITY)
table(restaurant_data$INSPECTOR)
