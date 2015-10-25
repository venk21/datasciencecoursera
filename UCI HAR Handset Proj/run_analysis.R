##Run_Analysis.R File Description

##Below script does the following. 
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



## Get Common variables as object - features
feature = read.table("./data/features.txt", header=FALSE)
colnames(feature)=c("featureid", "featurename", header=FALSE)

## Get Common variables as object - activity
activity = read.table("./data/activity_labels.txt")
colnames(activity)=c("activityid", "activitytype")

##Train dataset -- Read data from files 
trainsetx <- read.table("./data/train/X_train.txt", header=FALSE)
trainsety <- read.table("./data/train/Y_train.txt", header=FALSE)
trainsetsubject <- read.table("./data/train/subject_train.txt", header=FALSE)

##Adding column names for Training Datasets
##For X Trainset assign feature names
colnames(trainsetx)= feature[,2]
##For y Trainset assign as activity ID
colnames(trainsety)= "activityId"
colnames(trainsetsubject)= "subjectId"

##Create combined dataset for train based on the above three data set
trainingdataset <- cbind(trainsetsubject, trainsety, trainsetx)


##Test dataset -- Read data from files 
testsetx <- read.table("./data/test/X_test.txt", header=FALSE)
testsety <- read.table("./data/test/Y_test.txt", header=FALSE)
testsetsubject <- read.table("./data/test/subject_test.txt", header=FALSE)

##Adding column names for Training Datasets
##For X Trainset assign feature names
colnames(testsetx)= feature[,2]
##For y Trainset assign as activity ID
colnames(testsety)= "activityId"
colnames(testsetsubject)= "subjectId"

##Create combined dataset for train based on the above three data set
testdataset <- cbind(testsetsubject,testsety, testsetx )

##1. Merges the training and the test sets to create one data set.

##combining training and final data set into single data set
finaldataset <- rbind(trainingdataset, testdataset)


##2.Extracts only the measurements on the mean and standard deviation for each measurement

##Get all column names to extract measurements for mean and standard deviation
columnnamelist <- colnames(finaldataset)

##Identify Logical vector list based on grepl function for mean, std and required identifier columns
logicalvectorlist = (grepl("activity..",columnnamelist, ignore.case = TRUE) | 
     grepl("subject..",columnnamelist, ignore.case = TRUE) | 
          grepl("-mean..",columnnamelist, ignore.case = TRUE) |  
          grepl("-std..",columnnamelist, ignore.case = TRUE)
          & !grepl("-meanFreq..",columnnamelist, ignore.case = TRUE))

##Filter out all non wanted columns 
##keep only required mean and SD columns with other keys
finaldataset = finaldataset[logicalvectorlist==TRUE]

##3.Uses descriptive activity names to name the activities in the data set

##Use Descriptive names
finaldataset = merge(activity, finaldataset,  by.y = "activityId", 
                     by.x = "activityid", all.y = TRUE)

updatedColumnNameList = colnames(finaldataset)

##4.Appropriately labels the data set with descriptive variable names.
##Updating descriptive column names in the variable
for(i in 1:length(updatedColumnNameList))
{
     updatedColumnNameList[i]=gsub("-mean", "Mean", updatedColumnNameList[i])
     updatedColumnNameList[i]=gsub("-std", "StdDev", updatedColumnNameList[i])
     updatedColumnNameList[i]=gsub("^f", "Freq_", updatedColumnNameList[i])
     updatedColumnNameList[i]=gsub("^t", "Time_", updatedColumnNameList[i])
     updatedColumnNameList[i]=gsub("()", "", updatedColumnNameList[i])
     updatedColumnNameList[i]=gsub("BodyBody", "Body", updatedColumnNameList[i])
     updatedColumnNameList[i]=gsub("Mag", "Magnitude", updatedColumnNameList[i])
}

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
##Updating descriptive column names back to Final Dataset
colnames(finaldataset) = updatedColumnNameList
library(dplyr)
finaldatasetWOActivityType= finaldataset[,updatedColumnNameList!="activitytype"]

tidydataset = aggregate(select(finaldatasetWOActivityType, -(activityid:subjectId)), 
                        by=list(
                          activityid = finaldatasetWOActivityType$activityid
                        , subjectId = finaldatasetWOActivityType$subjectId)
                        ,mean)

##Use Descriptive names
tidydataset = merge(tidydataset, activity, by = "activityid", all.x = TRUE)

##Export the tidy data set
write.table(tidydataset, "./tidyData.txt", row.names = TRUE, sep="\t")