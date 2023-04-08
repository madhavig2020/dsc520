# Assignment: ASSIGNMENT 4.1 Scores Exercise
# Name: Ghanta, Madhavi
# Date: 2023-04-06

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Load the pastecs package
library(pastecs)

##If the current directory does not contain the data directory, set the working 
## directory to project root folder.(the folder should contain  data directory )
## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/mghan/Documents/dsc520")

## Load the `data/scores.csv` to Scores_df using read.csv
Scores_df <- read.csv("data/scores.csv")

##A professor has recently taught two sections of the same course with only one 
##difference between the sections. In one section, he used only examples taken
##from sports applications, and in the other section, he used examples taken 
##from a variety of application areas. The sports themed section was advertised
##as such; so students knew which type of section they were enrolling in. The 
##professor has asked you to compare student performance in the two sections 
##using course grades and total points earned in the course. You will need to 
##import the Scores.csv dataset that has been provided for you.

## Examine the structure of Scores_df using str()
## 1.What are the observational units in this study?
str(Scores_df)

# We have 38 observations with three variables.
## score and count are observational units in this study.

## 2.Identify the variables mentioned in the narrative paragraph and determine
## which are categorical and quantitative?
str(Scores_df)
summary(Scores_df)
#Section is categorical variable for the study.
#Count and Score are quantitative variables for the study.

#3.Create one variable to hold a subset of your data set that contains only the
## Regular Section and one variable for the Sports Section.
View(Scores_df)
reg_df <-Scores_df[which(Scores_df$Section=='Regular'),]
head(reg_df)
View(reg_df)
sport_df<-Scores_df[which(Scores_df$Section=='Sports'),]
head(sport_df)
View(sport_df)

# 4.Use the Plot function to plot each Sections scores and the number of
# students achieving that score. Use additional Plot Arguments to label the
# graph and give each axis an appropriate label. Once you have produced your
# Plots answer the following questions:

plot(reg_df$Score,reg_df$Count,type='h',xaxt="n",xlab="Score in Regular Section"
     ,ylab="Count of Students")
axis(1, at = seq(200, 400, by = 10), las=2)

plot(sport_df$Score,sport_df$Count,type='h',xaxt="n",xlab="Score in Sports Section",ylab="Count of Students")
axis(1, at = seq(200, 400, by = 10), las=2)

# 4.1. Comparing and contrasting the point distributions between the two section,
# looking at both tendency and consistency: Can you say that one section tended
# to score more points than the other? Justify and explain your answer.

#By looking at the two histograms plots, it seems that sports section students
#scored more higher marks > 300.

# 4.2. Did every student in one section score more points than every student in
# the other section? If not, explain what a statistical tendency means in this context.

stat.desc(reg_df[,1:2], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
stat.desc(sport_df[,1:2], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

bar <- ggplot(Scores_df, aes(Score,Count, fill = Section))
bar + stat_summary(fun = mean, geom = "bar", position="dodge",width = 8)+ facet_wrap( ~ Section)

#Total number of students in regular section is 290 and their mean score is 327.63
#Total number of students in sports section is 260 and their mean score is 307.37
#It looks like not every student in sports section score more points than every student in regular section.

# 4.3. What could be one additional variable that was not mentioned in the narrative
# that could be influencing the point distributions between the two sections?

#I think 'size of classes in each section' will be an additional variable could be influencing the point
#distributions between the two sections
