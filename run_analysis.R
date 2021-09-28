library("reshape2")

#Get and read data
filename <- "UCIdata.zip"
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Dataset"

if(!file.exists(filename)){
  download.file(url,filename, mode = "wb") 
}
if(!file.exists(dir)){
  unzip("UCIdata.zip", files = NULL, exdir=".")
}

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  

#1 Merges the training and the test sets to create one data set.
DataSet <- rbind(X_train,X_test)

#2 Extracts only the measurements on the mean and standard deviation for each measurement.
X <- grep("mean()|std()", features[, 2]) 
DataSet <- DataSet[,X]


#3 and 4 Uses descriptive activity names to name the activities in the data set + Appropriately labels the data set with descriptive variable names. 
VarNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(DataSet) <- VarNames[X]

Subject <- rbind(subject_train, subject_test)
names(Subject) <- 'subject'
Activity <- rbind(y_train, y_test)
names(Activity) <- 'activity'

DataSet <- cbind(Subject, Activity, DataSet)

ActData <- factor(DataSet$activity)
levels(ActData) <- activity_labels[,2]
DataSet$activity <- ActData


#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
DataSet2 <- melt(DataSet,(id.vars=c("subject","activity")))
FinalData <- dcast(DataSet2, subject + activity ~ variable, mean)
names(FinalData)[-c(1:2)] <- paste(names(FinalData)[-c(1:2)] )
write.table(FinalData, "TidyData.txt", sep = ",", row.names = FALSE)

