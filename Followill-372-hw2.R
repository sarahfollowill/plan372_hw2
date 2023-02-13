# PLAN 372 Homework 2
# Sarah Followill

# Load libraries
library(tidyverse)
library(lubridate)

# load data
# name data set object
inspections = read_csv("restaurant_inspections.csv")


ggplot(missings, aes(x=month_year, y=proportion_missing)) +
  geom_col()