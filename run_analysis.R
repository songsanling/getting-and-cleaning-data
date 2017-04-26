# 1. Merge the training and the test sets to create one data set.
#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/Users/sanling/Documents/UCI HAR Dataset/');

# Read in the train data from files and  assigin column names to the data imported above
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
# cCreate the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);
# do the same thing for testing data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";
# Merging the xTest, yTest and subjectTest data and combine training and test data to create a final data set
testData = cbind(yTest,subjectTest,xTest);
finalData = rbind(trainingData,testData);
colNames  = colnames(finalData); 


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];


# 3. Use descriptive activity names to name the activities in the data set
# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
colNames  = colnames(finalData); 


# 4. Appropriately label the data set with descriptive activity names. 
for (i in 1:length(colNames)) 
{
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
};
colnames(finalData) = colNames;


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];
# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
