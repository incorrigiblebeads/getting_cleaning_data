#run_analysis.R does the following:
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Downloads the file and puts the file in working directory
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Dataset.zip", mode="wb")

#Unzips the file. Unzipped files are in the folder UCI HAR Dataset

unzip(zipfile="./Dataset.zip",exdir="./.")

#Loads required packages
library(data.table)  											#efficient in handling large data as tables
library(dplyr)													#to aggregate variables to create the tidy data

#Reads name of the features and the name of the activities
feature.names <- read.table("UCI HAR Dataset/features.txt")						
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)		

#Reads training data
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity.train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
features.train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Reads test data
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity.test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
features.test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Merges the training and the test sets to create one data set
subject <- rbind(subject.train, subject.test)
activity <- rbind(activity.train, activity.test)
features <- rbind(features.train, features.test)

#Naming the columns
colnames(features) <- t(feature.names[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

#Merges the data
complete.data <- cbind(features,activity,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement. Extract the column indices that have either mean or std in them.
columns.means.SD <- grep(".*Mean.*|.*Std.*", names(complete.data), ignore.case=TRUE)

#Adds activity and subject columns to the list and look at the dimension of completeData
required.columns <- c(columns.means.SD, 562, 563)
dim(complete.data)

#Creates extracted data set with the selected columns in required.columns
extracted.data <- complete.data[,required.columns]

#Uses descriptive activity names to name the activities in the data set
#The activity field in extractedData is originally of numeric type. Change its type to character so that it can accept activity names from activityLabels
extracted.data$Activity <- as.character(extracted.data$Activity)
for (i in 1:6)
{
  extracted.data$Activity[extracted.data$Activity == i] <- as.character(activity.labels[i,2])
}
#Factor the activity variable in order to get the average from last step
extracted.data$Activity <- as.factor(extracted.data$Activity)

#Appropriately labels the data set with descriptive variable names
names(extracted.data)<-gsub("Acc", "Accelerometer", names(extracted.data))
names(extracted.data)<-gsub("Gyro", "Gyroscope", names(extracted.data))
names(extracted.data)<-gsub("BodyBody", "Body", names(extracted.data))
names(extracted.data)<-gsub("Mag", "Magnitude", names(extracted.data))
names(extracted.data)<-gsub("^t", "Time", names(extracted.data))
names(extracted.data)<-gsub("^f", "Frequency", names(extracted.data))
names(extracted.data)<-gsub("tBody", "TimeBody", names(extracted.data))
names(extracted.data)<-gsub("-mean()", "Mean", names(extracted.data), ignore.case = TRUE)
names(extracted.data)<-gsub("-std()", "STD", names(extracted.data), ignore.case = TRUE)
names(extracted.data)<-gsub("-freq()", "Frequency", names(extracted.data), ignore.case = TRUE)
names(extracted.data)<-gsub("angle", "Angle", names(extracted.data))
names(extracted.data)<-gsub("gravity", "Gravity", names(extracted.data))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

#Need Subject as a factor variable for average
extracted.data$Subject <- as.factor(extracted.data$Subject)
extracted.data <- data.table(extracted.data)

#Create tidyData as a data set with average for each activity and subject. 
tidy.dataset <- aggregate(. ~Subject + Activity, extracted.data, mean)
tidy.dataset <- tidyData[order(tidy.dataset$Subject,tidy.dataset$Activity),]

write.table(tidy.dataset, file = "Tidydata.txt", row.names = FALSE)