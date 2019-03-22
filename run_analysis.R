#---------------------------------------------------------------------------------------
# This file performs the following tasks:
# 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# The data set is "Human Activity Recognition Using Smartphones Data Set" which is available at
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#---------------------------------------------------------------------------------------
# Load (and install if needed) the dplyr package
if(!require("dplyr")) {install.packages("dplyr")}
library(dplyr)

#---------------------------------------------------------------------------------------
# Download zip file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset.zip")) {
    download.file(url, "UCI_HAR_Dataset.zip", mode = "wb")
}

# Unzip zip file
if (!file.exists("UCI HAR Dataset")) {
    unzip("UCI_HAR_Dataset.zip")
}

#---------------------------------------------------------------------------------------
# Load training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

# Load test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

# Load features
features <- read.table("UCI HAR Dataset/features.txt")
#head(features)

# Load activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
#head(activity_labels)

#---------------------------------------------------------------------------------------
## Task 1: Merges the training and the test sets to create one data set.

train <- cbind(subject_train, y_train, X_train)
test <- cbind(subject_test, y_test, X_test)
mergedData <- rbind(train, test)
#dim(mergedData)

#---------------------------------------------------------------------------------------
## Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.

# assign column names
names(mergedData) <- c("subjectId", "activity", as.character(features[, 2]))

# select required columns with means and standard deviations
requiredColumns <- grep("subjectId|activity|mean|std", names(mergedData))
mergedData <- mergedData[,requiredColumns]

#---------------------------------------------------------------------------------------
## Task 3: Uses descriptive activity names to name the activities in the data set.

mergedData$activity <- factor(mergedData$activity, levels = activity_labels[,1], labels = activity_labels[,2])

#---------------------------------------------------------------------------------------
## Task 4: Appropriately labels the data set with descriptive variable names.

variableNames <- names(mergedData) # get variable names

variableNames <- gsub("[-()]", "", variableNames)    # remove "-" and "()" from names
variableNames <- gsub("std", "StandardDeviation", variableNames)   # change "std" to "StandardDeviation"
variableNames <- gsub("mean", "Mean", variableNames) # change "mean" to "Mean"
variableNames <- gsub("BodyBody", "Body", variableNames) # Remove duplicate "Body" in the name

# change other abbreviations to full words
variableNames <- gsub("^f", "frequencyDomain", variableNames)
variableNames <- gsub("^t", "timeDomain", variableNames)
variableNames <- gsub("Acc", "Accelerometer", variableNames)
variableNames <- gsub("Gyro", "Gyroscope", variableNames)
variableNames <- gsub("Mag", "Magnitude", variableNames)
variableNames <- gsub("Freq", "Frequency", variableNames)

names(mergedData) <- variableNames # replace old column names with modified names

#---------------------------------------------------------------------------------------
## Task 5: Create a second, independent tidy data set with the average of each variable 
##         for each activity and each subject.

# group by subjectID and activity, and then summarize
mergedData_avg <- mergedData %>% group_by(subjectId, activity) %>% summarize_each(mean)

#---------------------------------------------------------------------------------------
# Write to file "tidyData.txt"
write.table(mergedData_avg, "tidyData.txt", row.names = FALSE, quote = FALSE)
