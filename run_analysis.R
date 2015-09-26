###Load required packages

library(dplyr)
library(data.table)
library(tidyr)

# Download zip file from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# unzip downloaded file in 'UCI HAR Dataset' folder 
# List all the files in 'UCI HAR Dataset'

file_path <- file.path("C:/Users/mzfphb/Desktop/Coursera/Assignment_3/Project_assgn","UCI HAR Dataset")
files<-list.files(file_path, recursive=TRUE)
files

# Read data from files into Activity, Features and Subject variables

ActivityTest  <- read.table(file.path(file_path, "test" , "Y_test.txt" ),header = FALSE)
ActivityTrain <- read.table(file.path(file_path, "train", "Y_train.txt"),header = FALSE)
SubjectTest  <- read.table(file.path(file_path, "test" , "subject_test.txt"),header = FALSE)
SubjectTrain <- read.table(file.path(file_path, "train", "subject_train.txt"),header = FALSE)
FeaturesTest  <- read.table(file.path(file_path, "test" , "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(file_path, "train", "X_train.txt"),header = FALSE)

# Read Activity names from activity_labels.txt

activityLabels <- read.table(file.path(file_path, "activity_labels.txt"),header = FALSE)

# Merge Training and Test data sets by rows and set varible names

dataSubject <- rbind(SubjectTrain, SubjectTest)
dataActivity<- rbind(ActivityTrain, ActivityTest)
dataFeatures<- rbind(FeaturesTrain, FeaturesTest)

names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
FeaturesNames <- read.table(file.path(file_path, "features.txt"),head=FALSE)
names(dataFeatures)<- FeaturesNames$V2

# 1. Merge data by column to create one data set (step 1 from coursera project assignment)

data <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

subsetFeaturesNames<-FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]
featureNames<-c(as.character(subsetFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=featureNames)

# 3.Uses descriptive activity names to name the activities in the data set

setnames(activityLabels, names(activityLabels), c("activity","activityName"))
Data <- merge(activityLabels, Data , by="activity", all.x=TRUE)
Data$activityName <- as.character(Data$activityName)

# 4.Appropriately labels the data set with descriptive variable names. 
names(Data)<-gsub("std()", "SD", names(Data))
names(Data)<-gsub("mean()", "MEAN", names(Data))
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.

dataAggr<- aggregate(. ~ subject + activityName, data = Data, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))
write.table(dataTable, "TidyData.txt", row.name=FALSE)
summary(dataTable)
