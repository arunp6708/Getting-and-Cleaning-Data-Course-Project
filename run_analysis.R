library(dplyr)

if(!file.exists("./Course 3 Final Project")) {
  dir.create("./Course 3 Final Project")
}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./Course 3 Final Project/projectdataset.zip")

unzip(zipfile = "./Course 3 Final Project/projectdataset.zip", exdir = "./Course 3 Final Project")

# 1. Merge the training and test datasets to create one data set

X_train <- read.table("./Course 3 Final Project/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./Course 3 Final Project/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./Course 3 Final Project/UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./Course 3 Final Project/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./Course 3 Final Project/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./Course 3 Final Project/UCI HAR Dataset/test/subject_test.txt")

features <- read.table("./Course 3 Final Project/UCI HAR Dataset/features.txt")
activities <- read.table("./Course 3 Final Project/UCI HAR Dataset/activity_labels.txt", col.names = c("activityID", "activityType"))

colnames(X_train) <- features[, 2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(X_test) <- features[, 2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

combinedtrain <- cbind(X_train, y_train, subject_train)
combinedtest <- cbind(X_test, y_test, subject_test)
mergeddataset <- rbind(combinedtrain, combinedtest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

meanandstd <- grepl("activityID|subjectID|mean\\(\\)|std\\(\\)", colnames(mergeddataset))
justMeanandStd <- mergeddataset[, meanandstd]

# 3. Uses descriptive activity names to name the activities in the data set
WithActivityNames <- merge(justMeanandStd, activities, by = "activityID", all.x = TRUE)

# 4. Appropriately labels the data set with descriptive variable names
names(WithActivityNames)<-gsub("Acc", "Accelerometer", names(WithActivityNames))
names(WithActivityNames)<-gsub("Gyro", "Gyroscope", names(WithActivityNames))
names(WithActivityNames)<-gsub("BodyBody", "Body", names(WithActivityNames))
names(WithActivityNames)<-gsub("Mag", "Magnitude", names(WithActivityNames))
names(WithActivityNames)<-gsub("^t", "Time", names(WithActivityNames))
names(WithActivityNames)<-gsub("^f", "Frequency", names(WithActivityNames))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyDataset <- WithActivityNames %>%
  group_by(subjectID, activityID, activityType) %>%
  summarise_all(mean)
write.table(TidyDataset, "TidyDataset.txt", row.names = FALSE)