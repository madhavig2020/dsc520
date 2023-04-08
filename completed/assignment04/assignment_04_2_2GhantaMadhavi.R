# Assignment: ASSIGNMENT 4.2.2  Housing Data Exercise
# Name: Ghanta, Madhavi
# Date: 2023-04-06

## Load the readxl package
library(readxl)

## Load the plyr package
library(plyr)

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/mghan/Documents/dsc520")

##We interact with a few datasets in this course, one you are already familiar
##with, the 2014 American Community Survey and the second is a Housing dataset,
##that provides real estate transactions recorded from 1964 to 2016.  For this
##exercise, you need to start practicing some data transformation steps – which 
##will carry into next week, as you learn some additional methods.  For this
##week, using either dataset (or one of your own – although I will let you know
##ahead of time that the Housing dataset is used for a later assignment, so not
##a bad idea for you to get more comfortable with now!), perform the following 
##data transformations:

## Load the `data/week-6-housing.xlsx` to
housing_df <- read_excel("data/week-7-housing.xlsx")
str(housing_df)
summary(housing_df)
head(housing_df)


# Use the apply function on a variable in your dataset
avg_price <- apply(housing_df['Sale Price'],2,mean)
avg_price

# Use the aggregate function on a variable in your dataset
#Calculate the average sales price for each city
groupCityPrice <- aggregate(housing_df$"Sale Price" ~ housing_df$ctyname,housing_df,mean)
head(groupCityPrice)

groupDatePrice <- aggregate(housing_df$"Sale Price" ~ housing_df$"Sale Date",housing_df,mean)
head(groupDatePrice)

#Calculate the total number of full baths in the homes by the year they were built
groupBathYear <- aggregate(housing_df$bath_full_count ~ housing_df$year_built,housing_df,sum)
head(groupBathYear)


# Use the plyr function on a variable in your dataset – more specifically, I want to see you split some data, perform a modification to the data, and then bring it back together
#install.packages("plyr")
library(plyr)

#Finding total number of rooms in each house

#Checking if any NA values for room information
any(is.na(housing_df$bedrooms))
any(is.na(housing_df$bath_full_count))
any(is.na(housing_df$bath_half_count))
any(is.na(housing_df$bath_3qtr_count))

#Take out NA citynames
housing_df$ctyname[is.na(housing_df$ctyname)] <- 'Not Stated'
head(housing_df)

#Only keep sales with city names listed
housing <- housing_df[housing_df$ctyname != 'Not Stated', ]
head(housing)

#Calculate number of rooms
housing$numRooms <-  with(housing,(bedrooms + bath_full_count + bath_half_count + bath_3qtr_count))
head(housing$numRooms)

# Check distributions of the data
library(ggplot2)

#Distribution of 'Sale Price' is negatively skewed -- more prices at the lower end of the distribution
#Would have to look at the real estate in these cities/zip codes around these dates and check the city mean/median price
#However, most of the houses are in the range from $0 - $1,000,000 which is pretty standard
#I need to look into the houses around $0, because that would be impossible for an object you are purchasing :)
ggplot(housing_df, aes(x=housing_df$'Sale Price')) + geom_histogram(bins=12)

#The distribution of the variable, year_built, is positively skewed
#Most of the houses were built anytime from 1970s to current day which I also think is pretty standard
#For the older houses, they will probably need some renovations eventually and may even need to be knocked down or rebuilt
#Many old houses, as old as the 1920s, would most likely not be livable anymore
ggplot(housing_df, aes(x=year_built)) + geom_histogram(bins = 15)


# Identify if there are any outliers
#Summary function
summary(housing_df)

#The house that has a sales price of $698 is an outlier
#Looking at the summary info for this variable, the minimum value is $698 and the maximum value is $4.4 million
#When looking at the data for the $698 dollar homes, they are relatively high in square feet and have multiple bedrooms,
#which is inplausible for a house that is so cheap
#Also, the first quartile for this variable starts at $460,000 which puts $698 way below the first range, marking it as an outlier
minPrice <- housing_df[housing_df$`Sale Price` == 698,]
minPrice$square_feet_total_living
minPrice$bedrooms

#Looking at 240 square foot total living
#This home is also an outlier ... 240 square feet total living with a sales price of $687,500 is crazy!
#From looking at the other homes that have a sales price of $687,500 or higher, 240 square feet is the minumum with the first quartile starting at 2700 square feet
#240 is quite different than 2700, which marks this home also as an outlier
minSqFt <- housing_df[housing_df$square_feet_total_living == 240,]
head(minSqFt)

squareFtPrice <- housing_df[housing_df$'Sale Price' >= 687500,]
summary(squareFtPrice$square_feet_total_living)

# Create at least 2 new variables

#New variable for total number of bathroom(s)
housing_df$totalBath <- with(housing_df,(bath_full_count + bath_half_count + bath_3qtr_count))
head(housing_df$totalBath)

any(is.na(housing_df$year_built))
any(is.na(housing_df$year_renovated))
#New variable for true age of house -- subtract current year, 2021, from the year that the house was built
housing_df$houseAge <- with(housing_df,(2021 - year_built))
head(housing_df$houseAge)
