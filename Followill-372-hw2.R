# PLAN 372 Homework 2
# Sarah Followill

# Load libraries
library(tidyverse)
library(lubridate)

# Load data
# Name dataset object
inspections = read_csv("restaurant_inspections.csv")


# 1. Visualize the overall distribution of inspection scores using a histogram. [1 point] 
# Create histogram and add aesthetics
# x is score, showing various scores of different inspected restaurants across the range 0-100
# Change bin width (width of bars to 1 to fit more bars in)
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

# Get rid of the NA value
# What is after the !is.na is the column name with the NAs in it
inspections_by_date = filter(inspections_by_date, !is.na(RESTAURANTOPENDATE))

# Plot the new table, with x as restaurant open date and y as mean score for that date
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

# Group mean inspection scores by city
cityscores = group_by(inspections, CITY) %>%
  summarize(SCORE=mean(SCORE))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
cityscores = filter(cityscores, !is.na(CITY))

# Display table of mean scores per city
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

# Create a frequency table to find count per inspector
countsDF <- table(df$inspections.INSPECTOR)

# View frequency table
view(countsDF)

# Use names function to match the inspector names in the countsDF with inspectors in the 
# inspections object, then assign the corresponding count to each inspector
df$count <- countsDF[match(df$inspections.INSPECTOR,  names(countsDF))]

# Make table of inspection count per inspector with only one result for each inspector
count_by_inspector = group_by(df, inspections.INSPECTOR) %>%
  summarize(inspections_per_inspector = mean(count))


# Calculate count of inspections by city
# Create data frame of cities
df_city <- data.frame(inspections$CITY)

# Create a frequency table to find inspection count per city
countsDF_city <- table(df_city$inspections.CITY)

# Use names function to match the city names in the countsDF_city with the city names in the 
# inspections object, then assign the corresponding count to each city
df_city$count <- countsDF_city[match(df_city$inspections.CITY,  names(countsDF_city))]

# Make a table of inspection count per city with only one result for each city
count_by_city = group_by(df_city, inspections.CITY) %>%
  summarize(inspections_per_city = mean(count))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
count_by_city = filter(count_by_city, !is.na(inspections_per_city))


# Calculate count of inspections by date of restaurant opening
# Create data frame of restaurant open dates
df_date <- data.frame(inspections$RESTAURANTOPENDATE)

# Create a frequency table to find count per each facility opening date
countsDF_date <- table(df_date$inspections.RESTAURANTOPENDATE)

# Use the names function to match the dates in the countsDF_date with the dates in the inspections object, 
# then assign the corresponding count to each date
df_date$count <- countsDF_date[match(df_date$inspections.RESTAURANTOPENDATE,  names(countsDF_date))]

# Make table of count per date with only one result for each date
count_by_date = group_by(df_date, inspections.RESTAURANTOPENDATE) %>%
  summarize(inspections_per_date = mean(count))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
count_by_date = filter(count_by_date, !is.na(inspections_per_date))


# The data currently have daily records. We want a yearly table.
# First, create a column with the year that we can group by. To do that, we parse
# the date, then use floor_date to get the beginning of the year.
count_by_date$inspections.RESTAURANTOPENYEAR = ymd_hms(count_by_date$inspections.RESTAURANTOPENDATE)

# Use the floor_date function to get the start of each year by setting the unit as "year"
count_by_date$inspections.RESTAURANTOPENYEAR = floor_date(count_by_date$inspections.RESTAURANTOPENYEAR, unit="year")

# Make a table of count per year with only one object for each year
# Create data frame of restaurant open dates
df_date_y <- data.frame(count_by_date$inspections.RESTAURANTOPENYEAR)

# Create a frequency table to find count per year
countsDF_date_y <- table(df_date_y$count_by_date.inspections.RESTAURANTOPENYEAR)

# Show frequency table
view(countsDF_date_y)



# 6. The data file contains records for many types of food-service facility (e.g. restaurants, 
# food trucks, etc.). Are the scores for restaurants higher than other types of facility? 

# Make new table of mean scores grouped by facility type
type_scores = group_by(inspections, FACILITYTYPE) %>%
  summarize(type_mean_score = mean(SCORE))

# Get rid of NA values
# What is after the !is.na is the column name with the NAs in it
type_scores = filter(type_scores, !is.na(FACILITYTYPE))

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
# x is score, showing various scores for restaurants only
ggplot(restaurants, aes(x=SCORE)) +
  geom_histogram(binwidth = 1)

# 7.2. Some restaurants have been in business much longer than others. Is there any trend in 
# terms of how highly older vs. newer restaurants score on their inspections? [0.5 points]
# Make new plot, organizing by date of restaurants to see trend

# Make new table of inspectors' mean scores grouped by restaurant open date for restaurants only.
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

# City names already cleaned in inspections object which was filtered to produce the restaurants object

# Create table of mean scores for restaurants only grouped by city
res_cityscores = group_by(restaurants, CITY) %>%
  summarize(SCORE=mean(SCORE))

# Display table
view(res_cityscores)


# 7.4. Wake County employs a whole team of inspectors. It is possible that some inspectors 
# may be more thorough than others. Do inspection scores for restaurants vary by inspector? [0.5 points] 

# Make new table grouped by inspectors and showing their mean scores for restaurants only
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

# Create a frequency table to find count of inspections per inspector
res_countsDF <- table(res_df$restaurants.INSPECTOR)

# Display frequency table
view(res_countsDF)

# Use the names function to match the inspector names in the res_countsDF with the same names in the 
# restaurants object, then assign the corresponding count to each inspector
res_df$count <- res_countsDF[match(res_df$restaurants.INSPECTOR,  names(res_countsDF))]

# Make table of inspection count grouped by inspector with only one result for each inspector
res_count_by_inspector = group_by(res_df, restaurants.INSPECTOR) %>%
  summarize(res_inspections_per_inspector = mean(count))


# Calculate count of restaurant only inspections by city
# Create data frame of cities
res_df_city <- data.frame(restaurants$CITY)

# Create a frequency table to find count per city
res_countsDF_city <- table(res_df_city$restaurants.CITY)

# Use the names function to match the city names in the res_countsDF_city with the same city names in the 
# restaurants object, then assign the corresponding count to each city
res_df_city$count <- res_countsDF_city[match(res_df_city$restaurants.CITY,  names(res_countsDF_city))]

# Make table of count per city with only one result for each city
res_count_by_city = group_by(res_df_city, restaurants.CITY) %>%
  summarize(res_inspections_per_city = mean(count))


# Calculate count of restaurant only inspections by date of restaurant opening
# Create data frame of restaurant open dates
res_df_date <- data.frame(restaurants$RESTAURANTOPENDATE)

# Create a frequency table to find count per date
res_countsDF_date <- table(res_df_date$restaurants.RESTAURANTOPENDATE)

# Use the names function to match the dates in the res_countsDF_date with the same dates in the 
# restaurants object, then assign the corresponding count to each date
res_df_date$count <- res_countsDF_date[match(res_df_date$restaurants.RESTAURANTOPENDATE,  names(res_countsDF_date))]

# Make table of count per date with only one result for each date
res_count_by_date = group_by(res_df_date, restaurants.RESTAURANTOPENDATE) %>%
  summarize(res_inspections_per_date = mean(count))

# The data currently has many rows, some for the same year, as it shows a row for each restaurant. 
# We can therefore also make a table showing the count of restaurants per year to make the table easier to read.
# First, create a column with the year that we can group by. To do that, we parse
# the date, then use floor_date to get the beginning of the year.
res_count_by_date$restaurants.RESTAURANTOPENYEAR = ymd_hms(res_count_by_date$restaurants.RESTAURANTOPENDATE)

# Use the floor_date function to get the start of each year
res_count_by_date$restaurants.RESTAURANTOPENYEAR = floor_date(res_count_by_date$restaurants.RESTAURANTOPENYEAR, unit="year")

# Making a table of inspection count per year
# Create data frame of restaurant open dates
res_df_date_y <- data.frame(res_count_by_date$restaurants.RESTAURANTOPENYEAR)

# Create a frequency table to find count per year
res_countsDF_date_y <- table(res_df_date_y$res_count_by_date.restaurants.RESTAURANTOPENYEAR)

# Show frequency table of count per year
view(res_countsDF_date_y)


