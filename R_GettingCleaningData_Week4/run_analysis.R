# load required libraries
library(data.table)
library(dplyr)
library(tidyverse)

# set working directory
my_path <- paste("/Users/edwardherring/Documents/",
                "GitHub/datasciencecoursera/",
                "R_GettingCleaningData_Week4",sep="")
setwd(my_path)
# read metadata
featureNames <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
# read training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
# read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
# combine training and test tables
subject <- rbind(subject_train, subject_test)
activity <-rbind(y_train, y_test)
features <- rbind(X_train, X_test)
# rename columns
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
# merge data
data_complete <- cbind(features, activity, subject)
# extract mean and std deviation
cols_with_mean_std <- grep(".*Mean.*|.*Std.*", names(data_complete), ignore.case = TRUE)
cols_required <- c(cols_with_mean_std, 562,563)
extracted_data <- data_complete[,cols_required]
# change data type for activity
extracted_data$Activity <- as.character(extracted_data$Activity)
for (i in 1:6) {
  extracted_data$Activity[extracted_data$Activity == i] <- as.character(activity_labels[i,2])
}
extracted_data$Activity <- as.factor(extracted_data$Activity)
# give column headers descriptive lnames
names(extracted_data)<-gsub("Acc", "Accelerometer", names(extracted_data))
names(extracted_data)<-gsub("Gyro", "Gyroscope", names(extracted_data))
names(extracted_data)<-gsub("BodyBody", "Body", names(extracted_data))
names(extracted_data)<-gsub("Mag", "Magnitude", names(extracted_data))
names(extracted_data)<-gsub("^t", "Time", names(extracted_data))
names(extracted_data)<-gsub("^f", "Frequency", names(extracted_data))
names(extracted_data)<-gsub("tBody", "TimeBody", names(extracted_data))
names(extracted_data)<-gsub("-mean()", "Mean", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-std()", "STD", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-freq()", "Frequency", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("angle", "Angle", names(extracted_data))
names(extracted_data)<-gsub("gravity", "Gravity", names(extracted_data))
# create independent tidy dataset with averages for each variable
extracted_data$Subject <- as.factor(extracted_data$Subject)
extracted_data <- data.table(extracted_data)
tidy_data <- aggregate(. ~Subject + Activity, extracted_data, mean)
tidy_data <- tidy_data[order(tidy_data$Subject,tidy_data$Activity),]
write.table(tidy_data,file = "Tidy.txt", row.names = FALSE)
