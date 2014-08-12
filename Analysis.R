# Analysis.R - Script that does the analysis for the project
# Submitted by Christopher Bortz
# For Reproducible Research Section 5 - Dr. Roger Peng
# August 4 - September 1, 2014
# Course Project 1 - Activity Monitoring

# Step 1 - Setup our environment
library(data.table)

# Step 2: Get the Data if we don't already have it
if(file.exists("./activity.zip") & !file.exists("./activity.csv")) {
    unzip("./activity.zip")
}

# Step 3 - Load the data
data <- read.csv("./activity.csv",
                 colClasses = c("integer", "Date", "integer"))
