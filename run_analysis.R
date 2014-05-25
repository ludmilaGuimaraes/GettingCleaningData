## Set the directory where UCI HAR Dataset is 
directory <- "./"

## Load files in the directory UCI HAR Dataset

xtrain <- read.table(paste0(directory, "UCI HAR Dataset/train/X_train.txt"))
xtest <- read.table(paste0(directory, "UCI HAR Dataset/test/X_test.txt"))
features <- read.table(paste0(directory, "UCI HAR Dataset/features.txt"))

## Merges the training and the test sets
mergeData <- rbind(xtrain,xtest)

## Set the column names with the second column of features 
colnames(mergeData) <- c(as.character(features[,2]))

## Extracts only the measurements on the mean and standard deviation for each measurement
selectedData <- mergeData[,c(grep("mean()",colnames(mergeData),fixed=TRUE),grep("std()",colnames(mergeData),fixed=TRUE))]

## Load Data from y_train.txt and y_test.txt

yTrain <- read.table(paste0(directory, "UCI HAR Dataset/train/y_train.txt"))
yTest <- read.table(paste0(directory, "UCI HAR Dataset/test/y_test.txt"))

## Uses descriptive activity names to name the activities in the data set.

activityData <- rbind(yTrain,yTest)
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

activityLabels[,2]<-as.character(activityLabels[,2])

## Appropriately labels the data set with descriptive activity names. 

for(i in 1:length(activityData[,1])){
  activityData[i,1]<-activityLabels[activityData[i,1],2]
}

## Get the data activity and join with the selected data 

dataWithActivity<-cbind(activityData,selectedData)

## Rename the new column

colnames(dataWithActivity)[1] <- "Activity"

## Finally, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjectTrain <- read.table(paste0(directory, "UCI HAR Dataset/train/subject_train.txt"))
subjectTest <- read.table(paste0(directory, "UCI HAR Dataset/test/subject_test.txt"))

subjectData <- rbind(subjectTrain,subjectTest)

tidyData <- cbind(subjectData,dataWithActivity)

colnames(tidyData)[1] <- "Subject"

finalTidyData <- aggregate( tidyData[,3] ~ Subject+Activity, data = tidyData, FUN= "mean" )

for(i in 4:ncol(tidyData)){
  finalTidyData[,i] <- aggregate( tidyData[,i] ~ Subject+Activity, data = tidyData, FUN= "mean" )[,3]
}

colnames(finalTidyData)[3:ncol(finalTidyData)] <- colnames(selectedData)

write.table(finalTidyData, file = "tidyData.txt")
