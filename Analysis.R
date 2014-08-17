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

# Step 9 - Plot the average by time interval
ggplot(data = stepsPerInterval,
       aes(x = interval,
           y = meanSteps)) + 
    geom_line(colour = "chartreuse4") + 
    xlab("Time Interval") + 
    ylab("Average Steps Taken") + 
    ggtitle("Average Steps Taken by 5 Min Interval (Oct - Nov, 2012)")

# Step 10 - Which interval has the highest average number of steps
mostActiveInterval <- stepsPerInterval[which.max(stepsPerInterval$meanSteps), ]

# Step 11 - How many rows have mising values
missingValues = sum(!complete.cases(activity$steps))
suppliedValues = sum(complete.cases(activity$steps)) # This is just a sanity check

# Step 12 - We need a couple of convenience functions, one will calculate
#           whether a date falls on the weekend and the other will 
#           impute missing data with the average for the activity
#           of that interval and day.

# Takes a date in POSIXct format and returns a logical whether this date falls on the weekend
weekend <- function(x) {
    return(wday(x) %in% c(1,7)) # Is this day either Saturday (7) or Sunday (1)?
}
dayType <- function(x) {
    if(weekend(x)) {
        return("weekend")
    } else {
        return("weekday")
    }
}

# If the value is missing it will take the mean of similarly grouped values
# This sweet function comes from:
# http://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr
# which comes from Hadley Wickam at:
# http://www.mail-archive.com/r-help@r-project.org/msg58289.html
impute.mean <- function(x) {
    return(replace(x, is.na(x), mean(x, na.rm = TRUE)))
}

# Step 13 - Impute missing values and add factor
# If the number of steps is NA, then impute the missing value by
# taking the mean over the other values of that interval for that day
activityImputed <- ddply(activity, 
                         ~ interval + wday(date),
                         transform,
                         steps = impute.mean(steps),  # Impute missing values
                         dayType = dayType(date))  # Create factor weegend/weekday
# ddply reorders according to grouping so we need to restore the original order
activityImputed <- arrange(activityImputed, date, interval) 

# Step 14 - Summarize the total number and average steps per day imputed
stepsPerDayImputed <- ddply(activityImputed, "date", summarize,
                            totalSteps = sum(steps),
                            meanSteps = mean(steps, na.rm = TRUE))

# Step 15 - Plot the total number of steps per day with imputed values
ggplot(data = stepsPerDayImputed,
       aes(x = date,
           y = totalSteps)) + 
    geom_bar(stat = "identity", colour = "chartreuse4", fill = "chartreuse4") + 
    xlab("Date") + 
    ylab("Total Steps Taken") + 
    ggtitle("Total Steps Taken by Date w/Imputation (Oct - Nov, 2012)")

# Step 16 - Calculate the mean and median of the total number of steps imputed
meanStepsImputed = mean(stepsPerDayImputed$totalSteps)
medianStepsImputed = median(stepsPerDayImputed$totalSteps)

# Step 17 - Summarize Steps by Time Interval faceted by dayType
stepsPerIntervalFaceted <- ddply(activityImputed, 
                                 ~ interval + dayType, 
                                 summarize,
                                 meanSteps = mean(steps))

# Step 18 - Compare Weekday vs Weekend Activity with Faceted Plot
ggplot(data = stepsPerIntervalFaceted,
       aes(x = interval,
           y = meanSteps)) + 
    geom_line(colour = "chartreuse4") + 
    facet_wrap(~ dayType, ncol = 1) +
    xlab("Time Interval") + 
    ylab("Average Steps Taken") + 
    ggtitle("Average Steps Taken by 5 Min Interval (Oct - Nov, 2012)")

