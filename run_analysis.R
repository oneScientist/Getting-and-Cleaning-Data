######################################
# 1. Load and merge data
######################################


# Redefine a read table function with a customized file path
our.read.table <- function(directory, suffix, fileName, ...){
    read.table(paste0(directory, fileName, suffix), ...)
}

# Load data 
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


######################################
# 2. Extract mean and standard deviation data
######################################

# Find the mean columns 
meanCols <- grep("mean", features_col[,2]) + 2

# Find the standard deviation columns 
stdCols <- grep("std", features_col[,2]) + 2

dataMeanStd <- mergedData[, c(1, 2,meanCols, stdCols)]



######################################
# 3. Name appropriatly the activities
######################################

# replace the activities by their labels
dataMeanStd$activity <- sapply(dataMeanStd$activity, function(x){ activity_labels[x,2]})


######################################
# 4. Label appropriatly the data
######################################

# Find the mean column names 
meanColNames <- grep("mean", features_col[,2], value = TRUE)

# Find the standard deviation column names 
stdColNames <- grep("std", features_col[,2], value = TRUE)

# Set the appropriate labels 
colnames2 <- names(dataMeanStd)

colnames2[1:length(meanColNames)+2] <- meanColNames
colnames2[1:length(stdColNames)+length(meanColNames)+2] <- stdColNames

# replace the actual labels by the appropriate ones
names(dataMeanStd) <- colnames2


######################################
# 5. create a data set with the average of each variable for each activity and each subject
######################################
averageByActivityAndSubject <- aggregate( . ~ (subject + activity) , FUN=function(x) mean(x, na.rm = TRUE), data=dataMeanStd)

# Create the tidy dataset
write.table(averageByActivityAndSubject, file = "tidydata.txt", row.names = FALSE)