# Analysis.R - Script that does the analysis for the project
# Submitted by Christopher Bortz
# For Reproducible Research Section 5 - Dr. Roger Peng
# August 4 - September 1, 2014
# Course Project 1 - Activity Monitoring

# Step 1 - Setup our environment
library(data.table)  #data.tables are SO much faster than data frames
library(lubridate) # Very handy date conversion functions
library(plyr) # Good for split-apply-combine processes
library(ggplot2) # My favorite default plotting library

# Step 2: Get the Data if we don't already have it
if(file.exists("./activity.zip") & !file.exists("./activity.csv")) {
    unzip("./activity.zip")
}

# Step 3 - Load the data
activity <- fread("./activity.csv")

# Step 4 - Transform Data (make activity$date a date vice a char)
activity$date <- ymd(activity$date) #Now we have POSIXct class dates

# Step 5 - Summarize the total number and average steps per day
stepsPerDay <- ddply(activity, "date", summarize,
                     totalSteps = sum(steps),
                     meanSteps = mean(steps, na.rm = TRUE))

# Step 6 - Plot the total number of steps per day
ggplot(data = stepsPerDay,
       aes(x = date,
           y = totalSteps)) + 
    geom_bar(stat = "identity", colour = "chartreuse4", fill = "chartreuse4") + 
    xlab("Date") + 
    ylab("Total Steps Taken") + 
    ggtitle("Total Steps Taken by Date (Oct - Nov, 2012)")

# Step 7 - Calculate the mean and median of the total number of steps
meanSteps = mean(stepsPerDay$totalSteps)
medianSteps = median(stepsPerDay$totalSteps)

# Step 8 - Summarize Steps by Time Interval
stepsPerInterval <- ddply(activity, "interval", summarize,
                          meanSteps = mean(steps, na.rm = TRUE))

# Step 9 - Which interval has the highest average number of steps
mostActiveInterval <- stepsPerInterval[which.max(stepsPerInterval$meanSteps), ]

# Step 10 - How many rows have mising values
missingValues = sum(!complete.cases(activity$steps))
suppliedValues = sum(complete.cases(activity$steps)) # This is just a sanity check

