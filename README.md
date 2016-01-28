---
title: "README"
author: "oneScientist"
date: "28 janvier 2016"
---

# Introduction

This document explains how the data have been handled, processed and how the tidydata.txt file has been generated.

# 1. Download data
Data have been downloaded in 01/20/2016 from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# 2. Load and merge data 

Before loading and merging data, we defined 2 functions: "our.read.table" and "load_data".

Redefine a read table function with a customized file path
```r
# Redefine a read table function with a customized file path
our.read.table <- function(directory, suffix, fileName, ...){
    read.table(paste0(directory, fileName, suffix), ...)
}
```
Load_data loads data from a directory (either train or test directory) and merges all data into a data frame where (consider the train data, but the same holds for test data)):
- subject: comes from "UCI HAR Dataset\train\subject_train.txt""
- activity: comes from "UCI HAR Dataset\train\y_train.txt"
- a 561-value vector for obtained activity values for the subject: from "UCI HAR Dataset\train\X_train.txt". This vector's labels are obtained from "UCI HAR Dataset\\activity_labels.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\body_acc_x_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\body_acc_y_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\body_acc_z_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\body_gyro_x_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\body_gyro_y_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\body_gyro_z_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\total_acc_x_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\total_acc_y_train.txt"
- a 128-value vector obtained from "UCI HAR Dataset\train\Inertial Signals\total_acc_z_train.txt"


```r
# Load data given a directory and a suffix for file extensions
load_data <- function(directory, suffix) {
    # subject + activity data
    subject <- our.read.table(directory, suffix, "subject", col.names = "subject")
    activity <- our.read.table(directory, suffix, "y", col.names = "activity")
    X <- our.read.table(directory, suffix, "X", colClasses = "numeric", col.names = features_col[,2])
    
    # Read data in the Inertial Signals folder
    directory <- paste0(directory, "Inertial Signals\\")
    
    body_acc_x <- our.read.table(directory, suffix, "body_acc_x", colClasses = "numeric")
    body_acc_y <- our.read.table(directory, suffix, "body_acc_y", colClasses = "numeric")
    body_acc_z <- our.read.table(directory, suffix, "body_acc_z", colClasses = "numeric")
    
    body_gyro_x <- our.read.table(directory, suffix, "body_gyro_x", colClasses = "numeric")
    body_gyro_y <- our.read.table(directory, suffix, "body_gyro_y", colClasses = "numeric")
    body_gyro_z <- our.read.table(directory, suffix, "body_gyro_z", colClasses = "numeric")
    
    total_acc_x <- our.read.table(directory, suffix, "total_acc_x", colClasses = "numeric")
    total_acc_y <- our.read.table(directory, suffix, "total_acc_y", colClasses = "numeric")
    total_acc_z <- our.read.table(directory, suffix, "total_acc_z", colClasses = "numeric")
    
    # Aggregate data
    data.frame(subject, activity, X,
               body_acc_x = body_acc_x, body_acc_y = body_acc_y, 
               body_acc_z = body_acc_z, total_acc_x = total_acc_x, 
               total_acc_y = total_acc_y, total_acc_z = total_acc_z,
               body_gyro_x = body_gyro_x, body_gyro_y = body_gyro_y, 
               body_gyro_z = body_gyro_z)
}
```


Read all train and test data and merge them into a single data frame "mergedData"

```r
# Read activity labels data
activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt", sep = " ", col.names = c("activity", "activity_label"))

# Read features data
features_col <- read.table("UCI HAR Dataset\\features.txt", sep = " ")

# Read train data
train_data <- load_data("UCI HAR Dataset\\train\\", "_train.txt")

# Read test data
test_data <- load_data("UCI HAR Dataset\\test\\", "_test.txt")

# Merge train and test data
mergedData <- rbind(train_data, test_data)

```


# 2. Extract mean and standard deviation data

- Find the mean columns in the features data

```r
# Find the mean columns 
meanCols <- grep("mean", features_col[,2]) + 2
```

- Find the standard deviation columns in the features data 

```r
# Find the standard deviation columns 
stdCols <- grep("std", features_col[,2]) + 2
```

- Extract from data all mean and standard deviation columns, and subject and activity columns 

```r
dataMeanStd <- mergedData[, c(1, 2,meanCols, stdCols)]
```


# 3. Name appropriatly the activities

Replace the activities by their labels from the activity_labels file

```r
dataMeanStd$activity <- sapply(dataMeanStd$activity, function(x){ activity_labels[x,2]})
```


# 4. Label appropriatly the data

- Find the mean column names 
- Find the standard deviation column names 
- Set the appropriate labels 
- Replace the actual labels by the appropriate ones

```r
meanColNames <- grep("mean", features_col[,2], value = TRUE)
stdColNames <- grep("std", features_col[,2], value = TRUE)

colnames2 <- names(dataMeanStd)

colnames2[1:length(meanColNames)+2] <- meanColNames
colnames2[1:length(stdColNames)+length(meanColNames)+2] <- stdColNames

names(dataMeanStd) <- colnames2
```


# 5. Create a data set with the average of each variable for each activity and each subject

Compute the average of each variable for each activity and each subject

```r
averageByActivityAndSubject <- aggregate( . ~ (subject + activity) , FUN=function(x) mean(x, na.rm = TRUE), data=dataMeanStd)
```

Create the tidy dataset file

```r
write.table(averageByActivityAndSubject, file = "tidydata.txt", row.names = FALSE)
```
