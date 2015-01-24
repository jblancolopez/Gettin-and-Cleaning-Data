# 1. Merge the training and the test sets to create one data set.
setwd('/Users/Jessica Giselle/Desktop/ProGCD')
  # Read data files
features <- read.table('./features.txt', header=F)
activity_labels <- read.table('./activity_labels.txt', header=F)
subject_train <- read.table('./subject_train.txt', header=F)
x_train <- read.table('./x_train.txt', header=F)
y_train <- read.table('./y_train.txt', header=F)
subject_test <- read.table('./subject_test.txt',header=F)
x_test <- read.table('./x_test.txt', header=F)
y_test <- read.table('./y_test.txt', header=F) 
  # Assign column names 
colnames(activity_labels) <- c('Activity_ID','Activity_Type')
colnames(subject_train) <- "Subject_ID"
colnames(x_train) <- features[,2]
colnames(y_train) <- "Activity_ID"
colnames(subject_test) = "Subject_ID"
colnames(x_test) <- features[,2]
colnames(y_test) <- "Activity_ID"
  # Merge data sets
Training_Data <- cbind(y_train, subject_train, x_train)
Test_Data <- cbind(y_test,subject_test, x_test)
Train_Test <- rbind(Training_Data, Test_Data)
colNames <- colnames(Train_Test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
logicalVector <- (grepl("Activity..",colNames) | grepl("Subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
Train_Test <- Train_Test[logicalVector==T]

# 3. Use descriptive activity names to name the activities in the data set
Train_Test <- merge(Train_Test, activity_labels, by='Activity_ID', all.x=T)
colNames <- colnames(Train_Test)

# 4. Appropriately label the data set with descriptive activity names.
for (i in 1:length(colNames)){
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(Train_Test) <- colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
NoActivityType <- Train_Test[,names(Train_Test) != 'Activity_Type'];
Tidy_Data <- aggregate(NoActivityType[,names(NoActivityType) != c('Activity_ID','Subject_ID')],by=list(Activity_ID=NoActivityType$Activity_ID, Subject_ID = NoActivityType$Subject_ID),mean);
Tidy_Data <- merge(Tidy_Data, activity_labels, by='Activity_ID',all.x=T);

# Export the Tidy_Data set
write.table(Tidy_Data, file = "./Tydy_Data.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
