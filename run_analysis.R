library(dplyr)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setwd("C:/Users/Dell XPS 9575 4K/Google Drive/My/Programming/DataObtainingAndCleaning/Project/")
fileName <- "data.zip"

#To create a transferable code that automatically download the file in any workstation
if(!file.exists(fileName)){
    download.file(fileURL, destfile = fileName, method = "curl")
    }
if(!file.exists("UCI HAR Dataset")){
    unzip(fileName)
}

#Load the activity labels and features

list.files("UCI HAR Dataset/")
labels <- read.table("UCI HAR Dataset/activity_labels.txt")
#Each person performed six activities (WALKING, WALKING_UPSTAIRS, 
#WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
tbl_df(labels)
#Convert factor into characters
labels[,2] <- as.character(labels[,2])


features <- read.table("UCI HAR Dataset/features.txt")
#Using its embedded accelerometer and gyroscope, we captured 3-axial linear
#acceleration and 3-axial angular velocity at a constant rate of 50Hz.
#The sensor acceleration signal has gravitational and body motion components
tbl_df(features)
#Also convert factor into characters
features[,2] <- as.character(features[,2])

#Before loading the datasets, select the mean and std first because the dataset
#is too big.

#Need to review usage rule of regular expressions
featuresWanted <- grep(pattern = ".*mean.*|.*std.*", features[,2])
#Select out the actual feature names that contain "mean" and "std". This is to 
#prepare the names for labeling the combined table in the later step, since 
#the original table did not contain column names.
featuresWanted.names <- features[featuresWanted,2]
#Clean up the format of data feature names
featuresWanted.names = gsub(pattern = "-mean", replacement = "Mean", x = featuresWanted.names)
featuresWanted.names = gsub(pattern = "-std", replacement = "Std", x = featuresWanted.names)
featuresWanted.names <- gsub(pattern = "[-()]", replacement = "", x = featuresWanted.names)

#Load the datasets
#Opened the README.txt file allowed me to see what each file contains

#Combine the feature labels and subjects together for the training data
trainSet <- read.table("UCI HAR Dataset/train/X_train.txt")
#select out the features wanted from trainSet
trainSet <- trainSet[featuresWanted] #I don't know why, but it automatically selected from column, not row.
trainLabels <- read.table("UCI HAR Dataset/train/y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainCombined <- cbind(trainSubjects, trainLabels, trainSet)

#Do the same for the test group data
testSet <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testLabels <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testCombined <- cbind(testSubjects, testLabels, testSet)

#Now we got the mean and std from each group (train and test), we can combine 
#them into one table

allData <- rbind(trainCombined, testCombined)
#I found that I wasn't able to view the allData because there are duplicates in 
#column labels. 
#So we need to give column names to the allData
colnames(allData) <- c("subjects", "activity", featuresWanted.names)
#Now we can view the combined table
tbl_df(allData)

#We need to label the activities with descriptive activity names, instead of numbers
allData$activity <- factor(x = allData$activity, levels = labels[,1], labels = labels[,2])

#Also turn subject into factors
allData$subjects <- as.factor(allData$subjects)
View(tbl_df(allData))

#According to the requirement, I have to take the mean of each subject's individual activity
#Use melt function from the reshape2 package to transform each variable into one col.
install.packages("reshape2")
library(reshape2)
allData.melt <- melt(allData, id = c("subjects", "activity"))
View(tbl_df(allData.melt))
#Then use the dcast function to take the average of each activity's mean variable value
allData.mean <- dcast(allData.melt, subjects+activity~variable, mean)
View(allData.mean)
#Export data table as txt named tidyData.txt
write.table(allData.mean,"tidyData.txt", row.names = FALSE, quote = FALSE)
