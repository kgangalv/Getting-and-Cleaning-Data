# The ource of data is: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# 1. Merging the training and test sets for creating one data set

train_temp <- read.table("train/X_train.txt")
test_temp <- read.table("test/X_test.txt") 
X_data <- rbind(train_temp, test_temp)

train_temp <- read.table("train/y_train.txt")
test_temp <- read.table("test/y_test.txt") 
Y_data <- rbind(train_temp, test_temp)

train_temp <- read.table("train/subject_train.txt")
test_temp <- read.table("test/subject_test.txt") 
Subject_data <- rbind(train_temp, test_temp)

# Extracting measurements on the mean and standard deviation for each measurement


features <- read.table("features.txt")
index_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X_data <- X_data[, index_of_good_features]
names(X_data) <- features[index_of_good_features, 2]
names(X_data) <- gsub("\\(|\\)", "", names(X_data))
names(X_data) <- tolower(names(X_data))

# 3. Uses descriptive activity names to name the activities in the data set.

activity <- read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
Y_data[, 1] = activity[Y_data[ , 1], 2]
names(Y_data) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(Subject_data) <- "subject"
clean <- cbind(Subject_data, Y_data, X_data)
write.table(clean, "merged_clean_and_tidy_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects <- unique(Subject_data)[, 1]
numSubjects <- length(unique(Subject_data)[, 1])
numActivities <- length(activity[, 1])
numColumns <- dim(clean)[2]
result <- clean[1:(numSubjects*numActivities), ]
row <- 1
for (s in 1:numSubjects){
  for (a in 1:numActivities){
    result[row, 1] <- uniqueSubjects[s]
    result[row, 2] <- activity[a, 2]
    temp <- clean[clean$subject == s & clean$activity == activity[a, 2], ]
    result[row, 3:numColumns] <- colMeans(temp[, 3:numColumns])
    row <- row + 1
  }
}
write.table(result, "data_set_with_the_averages.txt")