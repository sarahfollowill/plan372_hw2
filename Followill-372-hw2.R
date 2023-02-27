# PLAN 372 Homework 2
# Sarah Followill

# Load libraries
library(tidyverse)
library(lubridate)

# load data
# name data set object
inspections = read_csv("restaurant_inspections.csv")


# 1. Visualize the overall distribution of inspection scores using a histogram. [1 point] 
# Create histogram and add aesthetics
# x is score, showing counts of various scores across the range 0-100
ggplot(inspections, aes(x=SCORE)) +
  geom_histogram(binwidth = 1)



# 2. Some restaurants have been in business much longer than others. Is there any trend in 
# terms of how highly older vs. newer restaurants score on their inspections? [0.5 points]
# Make new plot, organizing by date of restaurants to see trend

# Make new table of inspectors' mean scores by restuarant open date
inspections_by_date = group_by(inspections, RESTAURANTOPENDATE) %>%
  summarize(mean_score_by_date = mean(SCORE))

# Display the new table
view(inspections_by_date)

# Plot the new table
ggplot(inspections_by_date, aes(x=RESTAURANTOPENDATE, y=mean_score_by_date)) +
  geom_col()



# 3. Wake County is the most populous county in North Carolina, and there are many cities 
# in it. Do the inspection scores vary by city? Note that the city column contains some 
# differently spelled city names; make sure to clean those up so that there is only one 
# estimated value per city. The recode function that we used for creating a 
# weekend/weekday variable in the SFpark exercise will be useful here, and you may also 
# be interested in the str_to_upper function. [1 point] 

# Go through list of cities in alphabetical order and find spelling and case mismatches
# Use recode to make all cities correct spelling and case
inspections$CITY = recode(inspections$CITY, "Apex"="APEX", "Cary"="CARY", "Fuquay Varina"="FUQUAY VARINA",
                          "FUQUAY-VARINA"="FUQUAY VARINA", "Fuquay-Varina"="FUQUAY VARINA",
                          "Garner"="GARNER", "Holly Springs"="HOLLY SPRINGS", "HOLLY SPRING"="HOLLY SPRINGS",
                          "Morrisville"="MORRISVILLE", "Raleigh"="RALEIGH", "RTP"="RESEARCH TRIANGLE PARK",
                          "Wake Forest"="WAKE FOREST", "Zebulon"="ZEBULON",)

# now, look at mean scores by city
cityscores = group_by(inspections, CITY) %>%
  summarize(SCORE=mean(SCORE))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
cityscores = filter(cityscores, !is.na(CITY))

#display
view(cityscores)



# 4. Wake County employs a whole team of inspectors. It is possible that some inspectors 
# may be more thorough than others. Do inspection scores vary by inspector? [0.5 points] 

# Make new table of inspectors and their mean scores
inspector_scores = group_by(inspections, INSPECTOR) %>%
  summarize(inspector_mean_score = mean(SCORE))

# Display the new table
view(inspector_scores)

# Plot the new table
# Putting inspector names in the y aesthetic lets you read the full name
ggplot(inspector_scores, aes(x=inspector_mean_score, y=INSPECTOR)) +
  geom_col()



# 5. It is possible that some extreme results from the previous questions are due to small 
#sample sizes in a particular city, for a particular inspector, or in a particular time period. 
#Look at the sample sizes in each of your groups. Do you think this is an explanation for 
#the results you came to above?

# Calculate count of inspections by inspector
# Create data frame of inspectors
df <- data.frame(inspections$INSPECTOR)

# create a frequency table to find count per inspector
countsDF <- table(df$inspections.INSPECTOR)

view(countsDF)

# use names to match the inspector names in the countsDF, then assign 
# the corresponding count to each inspector
df$count <- countsDF[match(df$inspections.INSPECTOR,  names(countsDF))]

# Make table of count per inspector with only one object for each inspector
count_by_inspector = group_by(df, inspections.INSPECTOR) %>%
  summarize(inspections_per_inspector = mean(count))


# Calculate count of inspections by city
# Create data frame of cities
df_city <- data.frame(inspections$CITY)

# create a frequency table to find count per city
countsDF_city <- table(df_city$inspections.CITY)

# use names to match the city names in the countsDF_city, then assign 
# the corresponding count to each city
df_city$count <- countsDF_city[match(df_city$inspections.CITY,  names(countsDF_city))]

# Make table of count per city with only one object for each city
count_by_city = group_by(df_city, inspections.CITY) %>%
  summarize(inspections_per_city = mean(count))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
count_by_city = filter(count_by_city, !is.na(inspections_per_city))


# Calculate count of inspections by date of restaurant opening
# Create data frame of restaurant open dates
df_date <- data.frame(inspections$RESTAURANTOPENDATE)

# create a frequency table to find count per date
countsDF_date <- table(df_date$inspections.RESTAURANTOPENDATE)

# use names to match the dates in the countsDF_date, then assign 
# the corresponding count to each date
df_date$count <- countsDF_date[match(df_date$inspections.RESTAURANTOPENDATE,  names(countsDF_date))]

# Make table of count per city with only one object for each city
count_by_date = group_by(df_date, inspections.RESTAURANTOPENDATE) %>%
  summarize(inspections_per_date = mean(count))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
count_by_date = filter(count_by_date, !is.na(inspections_per_date))


#Work Zone, trying to get just years
# The data currently have hourly records. We want a monthly plot.
# First, create a column with the month that we can group by. To do that, we parse
# the date, then use floor_date to get the beginning of the month.
# lubridate has date parsing functions names things like mdy_hms for
# month/day/year, hours:minutes:seconds format. Inspect the Time column and figure
# out which function to use here.
count_by_date$inspections.RESTAURANTOPENYEAR = ymd_hms(count_by_date$inspections.RESTAURANTOPENDATE)

# Use the floor_date function to get the start of each month
# Working until here
count_by_date$inspections.RESTAURANTOPENYEAR = floor_date(count_by_date$inspections.RESTAURANTOPENYEAR, unit="year")

# Make table of count per year with only one object for each year
# Not working
count_by_date$inspections.RESTAURANTOPENYEAR = group_by(count_by_date, inspections.RESTAURANTOPENYEAR, unit = "year") %>%
  summarize(count_by_date = sum(count))


# Trying another way to make table of count per year with only one object for each year
# Create data frame of restaurant open dates
df_date_y <- data.frame(count_by_date$inspections.RESTAURANTOPENYEAR)

# create a frequency table to find count per date
countsDF_date_y <- table(df_date_y$count_by_date.inspections.RESTAURANTOPENYEAR)

# This works, shows frequency table
view(countsDF_date_y)


# use names to match the city names in the countsDF_city, then assign 
# the corresponding count to each city
# Stops working here, getting NA values
df_date_y$count <- countsDF_date_y[match(df_date_y$count_by_date.inspections.RESTAURANTOPENYEAR,  names(countsDF_date_y))]

# Make table of count per city with only one object for each city
count_by_date_y = group_by(df_date_y, count_by_date.inspections.RESTAURANTOPENYEAR) %>%
  summarize(inspections_per_date_y = mean(count))




# 6. The data file contains records for many types of food-service facility (e.g. restaurants, 
# food trucks, etc.). Are the scores for restaurants higher than other types of facility? 

# Make new table of restaurant types and their mean scores
type_scores = group_by(inspections, FACILITYTYPE) %>%
  summarize(type_mean_score = mean(SCORE))

# Display the new table
view(type_scores)

# Plot the new table
# Putting facility type names in the y aesthetic lets you read the full name
ggplot(type_scores, aes(x=type_mean_score, y=FACILITYTYPE)) +
  geom_col()


# 7. Since restaurants are where the general public is most likely to interact with the food-
#service system, Wake County Public Health is particularly interested in sanitation in 
#restaurants. Repeat the analyses above (1-5) for restaurants specifically.

#Make new table of data only from restaurants by filtering original dataset "inspections"
#to only include objects where the facility type is restaurant.
restaurants = filter(inspections, FACILITYTYPE == "Restaurant")


# 7.1. Visualize the overall distribution of inspection scores for restaurants only using a histogram. 
# Create histogram and add aesthetics
# x is score, showing counts of various scores for restaurants across the range 0-100
ggplot(restaurants, aes(x=SCORE)) +
  geom_histogram(binwidth = 1)

# 7.2. Some restaurants have been in business much longer than others. Is there any trend in 
# terms of how highly older vs. newer restaurants score on their inspections? [0.5 points]
# Make new plot, organizing by date of restaurants to see trend

# Make new table of inspectors' mean scores by restaurant open date for restaurants only.
res_inspections_by_date = group_by(restaurants, RESTAURANTOPENDATE) %>%
  summarize(res_mean_score_by_date = mean(SCORE))

# Display the new table
view(res_inspections_by_date)

# Plot the new table
ggplot(res_inspections_by_date, aes(x=RESTAURANTOPENDATE, y=res_mean_score_by_date)) +
  geom_col()

# 7.3. Wake County is the most populous county in North Carolina, and there are many cities 
# in it. Do the inspection scores vary by city? Note that the city column contains some 
# differently spelled city names; make sure to clean those up so that there is only one 
# estimated value per city. The recode function that we used for creating a 
# weekend/weekday variable in the SFpark exercise will be useful here, and you may also 
# be interested in the str_to_upper function. [1 point] 

# City names already cleaned

# now, look at mean scores for restaurants only by city and display
res_cityscores = group_by(restaurants, CITY) %>%
  summarize(SCORE=mean(SCORE))

view(res_cityscores)


# 7.4. Wake County employs a whole team of inspectors. It is possible that some inspectors 
# may be more thorough than others. Do inspection scores vary by inspector? [0.5 points] 

# Make new table of inspectors and their mean scores for restaurants only
res_inspector_scores = group_by(restaurants, INSPECTOR) %>%
  summarize(res_inspector_mean_score = mean(SCORE))

# Display the new table
view(res_inspector_scores)

# Plot the new table
# Putting inspector names in the y aesthetic lets you read the full name
ggplot(res_inspector_scores, aes(x=res_inspector_mean_score, y=INSPECTOR)) +
  geom_col()


#7.5 It is possible that some extreme results from the previous questions are due to small 
#sample sizes in a particular city, for a particular inspector, or in a particular time period. 
#Look at the sample sizes in each of your groups. Do you think this is an explanation for 
#the results you came to above for restaurants only?

# Calculate count of restaurant only inspections by inspector
# Create data frame of inspectors
res_df <- data.frame(restaurants$INSPECTOR)

# create a frequency table to find count per inspector
res_countsDF <- table(res_df$restaurants.INSPECTOR)

view(res_countsDF)

# use names to match the inspector names in the res_countsDF, then assign 
# the corresponding count to each inspector
res_df$count <- res_countsDF[match(res_df$restaurants.INSPECTOR,  names(res_countsDF))]

# Make table of count per inspector with only one object for each inspector
res_count_by_inspector = group_by(res_df, restaurants.INSPECTOR) %>%
  summarize(res_inspections_per_inspector = mean(count))


# Calculate count of restaurant only inspections by city
# Create data frame of cities
res_df_city <- data.frame(restaurants$CITY)

# create a frequency table to find count per city
res_countsDF_city <- table(res_df_city$restaurants.CITY)

# use names to match the city names in the res_countsDF_city, then assign 
# the corresponding count to each city
res_df_city$count <- res_countsDF_city[match(res_df_city$restaurants.CITY,  names(res_countsDF_city))]

# Make table of count per city with only one object for each city
res_count_by_city = group_by(res_df_city, restaurants.CITY) %>%
  summarize(res_inspections_per_city = mean(count))


# Calculate count of restaurant only inspections by date of restaurant opening
# Create data frame of restaurant open dates
res_df_date <- data.frame(restaurants$RESTAURANTOPENDATE)

# create a frequency table to find count per date
res_countsDF_date <- table(res_df_date$restaurants.RESTAURANTOPENDATE)

# use names to match the dates in the res_countsDF_date, then assign 
# the corresponding count to each city
res_df_date$count <- res_countsDF_date[match(res_df_date$restaurants.RESTAURANTOPENDATE,  names(res_countsDF_date))]

# Make table of count per city with only one object for each city
res_count_by_date = group_by(res_df_date, restaurants.RESTAURANTOPENDATE) %>%
  summarize(res_inspections_per_date = mean(count))


#Work Zone, trying to get just years
# The data currently have hourly records. We want a monthly plot.
# First, create a column with the month that we can group by. To do that, we parse
# the date, then use floor_date to get the beginning of the month.
# lubridate has date parsing functions names things like mdy_hms for
# month/day/year, hours:minutes:seconds format. Inspect the Time column and figure
# out which function to use here.
res_count_by_date$restaurants.RESTAURANTOPENYEAR = ymd_hms(res_count_by_date$restaurants.RESTAURANTOPENDATE)

# Use the floor_date function to get the start of each month
# Working until here
res_count_by_date$restaurants.RESTAURANTOPENYEAR = floor_date(res_count_by_date$restaurants.RESTAURANTOPENYEAR, unit="year")

# Make table of count per year with only one object for each year
# Not working
res_count_by_date$restaurants.RESTAURANTOPENYEAR = group_by(res_count_by_date, restaurants.RESTAURANTOPENYEAR, unit = "year") %>%
  summarize(res_count_by_date = sum(count))


# Trying another way to make table of count per year with only one object for each year
# Create data frame of restaurant open dates
res_df_date_y <- data.frame(res_count_by_date$restaurants.RESTAURANTOPENYEAR)

# create a frequency table to find count per date
res_countsDF_date_y <- table(res_df_date_y$res_count_by_date.restaurants.RESTAURANTOPENYEAR)

# This works, shows frequency table
view(res_countsDF_date_y)

# use names to match the city names in the countsDF_city, then assign 
# the corresponding count to each city
# Stops working here, getting NA values
res_df_date_y$count <- res_countsDF_date_y[match(res_df_date_y$res_count_by_date.restaurants.RESTAURANTOPENYEAR,  names(res_countsDF_date_y))]

# Make table of count per city with only one object for each city
res_count_by_date_y = group_by(res_df_date_y, res_count_by_date.restaurants.RESTAURANTOPENYEAR) %>%
  summarize(res_inspections_per_date_y = mean(count))

