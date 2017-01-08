# README.md

Jan/08/2017

This repository is the final assignment of Coursera Data Science Getting Course and Cleaning Data by Fernando Goulart 

***
There is just one script called run_analysis.r

Steps:

1. Setup
* First of all, working dir is set as the same dir of UCI Har Dataset (please customize to your own UCI Har directory)
* Load column names (features.txt) to features dataframe

2. Test Setup
* Load subjects of test to test_subjects dataframe
* Load test data (X_test.txt) to test dataframe
* Load test activities (y_test.txt) to test_activity dataframe

3. Test Dataset
* Set test dataframe column names from features
* Include a column (test$DataType) marking data as Test Data (Te)
* Include a column with subjects ids  (test$subject)
* Include a column with activity (test$act)

4. Train Setup
* Load train subjects (subject_train.txt) to train_subjects dataframe
* Load train data (X_train.txt) to train dataframe
* Load train activity (y_train.txt) to train_activity dataframe

5. Train Dataset
* Set train dataframe columns from features
* Include a column (train$DataType) marking data as Train Data (Tr)
* Include a column with subjects ids (train$subject)
* Include a column with activity (train$act)

6. Merge data
* Merge test and train data with rbind function creating mergedData dataframe

7. Select only mean and standard deviation variables/columns
* Subset features dataframe using grepl and subset functions creating means_stds dataframe
* Using dataframe created on previous step (means_stds) subset mergedData creating meansStdData dataframe

8. Include descriptive activity variable
* Include a new column (meanStdData$activity) with descriptive activity variable
* Drop previous integer activity column 

9. Label variable names with more intuitive names
* Creates a newColumnNames array with new names
* Change labels using colnames function

10. Final Dataframe with averages
* Melt data usign melt function from reshape2 library
* Calculate means using dcast function
* Export to text file using write.table function


***
run_analysis.r
```r
#set working directory as UCI HAR Dataset directory
setwd('/Users/fegoulart/DataScience/datasciencecoursera/GetCleanData/week4/UCI HAR Dataset/')

#Load features
features <- read.table("features.txt")

#Test Dataset

#load test_subjects
test_subjects <- read.table("./test/subject_test.txt")

#load test data
test <- read.table("./test/X_test.txt")

#load test activity
test_activity <- read.table("./test/y_test.txt")

#set column names from features
colnames(test) <- features[,2]

#mark data as Test Data
test$DataType <- "Te"

#add subjects to test dataframe
test$subject <- test_subjects[,1]

#add activity
test$act <- test_activity[,1]

#Train Dataset

#load train_subjects
train_subjects <- read.table("./train/subject_train.txt")

#load train data
train <- read.table("./train/X_train.txt")

#load train activity
train_activity <- read.table("./train/y_train.txt")

#set column names from features
colnames(train) <- features[,2]

#mark data as Test Data
train$DataType <- "Tr"

#add subjects to test dataframe
train$subject <- train_subjects[,1]

#add activity
train$act <- train_activity[,1]

#Merge test and train dataframes
mergedData <- rbind(test,train)

#######END OF 1#########
#######2 ########

#get only [M][m]ean and std column names 
means_stds <-  subset(features$V2, grepl("std",features$V2) | grepl("ean",features$V2))

#filter only Mean and Std data
meanStdData <- mergedData[, which(names(mergedData) %in% means_stds | names(mergedData) %in% c("subject","act", "DataType") )]

#### END OF 2 ####
##### 3 #####

#Descriptive activity 
meanStdData$activity = ifelse(meanStdData$act== 1, 'WALKING', ifelse(meanStdData$act== 2, 'WALKING_UPSTAIRS',ifelse(meanStdData$act== 3, 'WALKING_DOWNSTAIRS',ifelse(meanStdData$act== 4,'SITTING',ifelse(meanStdData$act== 5,'STANDING',ifelse(meanStdData$act== 6,'LAYING','NA' ))))))
meanStdData <- meanStdData[, !(names(meanStdData) %in% c("act") )]

newColumnNames <- c("MeanXAxisBodyAccelerometer",
                    "MeanYAxisBodyAccelerometer",
                    "MeanZAxisBodyAccelerometer",
                    "StandardDeviationXAxisBodyAccelerometer",
                    "StandardDeviationYAxisBodyAccelerometer",
                    "StandardDeviationZAxisBodyAccelerometer",
                    "MeanXAxisGravityAccelerometer",
                    "MeanYAxisGravityAccelerometer",
                    "MeanZAxisGravityAccelerometer",
                    "StandardDeviationXAxisGravityAccelerometer",
                    "StandardDeviationYAxisGravityAccelerometer",
                    "StandardDeviationZAxisGravityAccelerometer",
                    "MeanXAxisJerkBodyAccelerometer",
                    "MeanYAxisJerkBodyAccelerometer",
                    "MeanZAxisJerkBodyAccelerometer",
                    "StandardDeviationXAxisJerkBodyAccelerometer",
                    "StandardDeviationYAxisJerkBodyAccelerometer",
                    "StandardDeviationZAxisJerkBodyAccelerometer",
                    "MeanXAxisBodyGyroscope",
                    "MeanYAxisBodyGyroscope",
                    "MeanZAxisBodyGyroscope",
                    "StandardDeviationXAxisBodyGyroscope",
                    "StandardDeviationYAxisBodyGyroscope",
                    "StandardDeviationZAxisBodyGyroscope",
                    "MeanXAxisJerkBodyGyroscope",
                    "MeanYAxisJerkBodyGyroscope",
                    "MeanZAxisJerkBodyGyroscope",
                    "StandardDeviationXAxisJerkBodyGyroscope",
                    "StandardDeviationYAxisJerkBodyGyroscope",
                    "StandardDeviationZAxisJerkBodyGyroscope",
                    "MeanEuclideanNormBodyAccelerometer",
                    "StandardDeviationEuclideanNormBodyAccelerometer",
                    "MeanEuclideanNormGravityAccelerometer",
                    "StandardDeviationEuclideanNormGravityAccelerometer",
                    "MeanEuclideanNormJerkBodyAccelerometer",
                    "StandardDeviationEuclideanNormJerkBodyAccelerometer",
                    "MeanEuclideanNormBodyGyroscope",
                    "StandardDeviationEuclideanNormBodyGyroscope",
                    "MeanEuclideanNormJerkBodyGyroscope",
                    "StandardDeviationEuclideanNormJerkBodyGyroscope",
                    "MeanXAxisBodyAccelerometerFourier",
                    "MeanYAxisBodyAccelerometerFourier",
                    "MeanZAxisBodyAccelerometerFourier",
                    "StandardDeviationXAxisBodyAccelerometerFourier",
                    "StandardDeviationYAxisBodyAccelerometerFourier",
                    "StandardDeviationZAxisBodyAccelerometerFourier",
                    "MeanFrequencyXAxisBodyAccelerometerFourier",
                    "MeanFrequencyYAxisBodyAccelerometerFourier",
                    "MeanFrequencyZAxisBodyAccelerometerFourier",
                    "MeanXAxisJerkBodyAccelerometerFourier",
                    "MeanYAxisJerkBodyAccelerometerFourier",
                    "MeanZAxisJerkBodyAccelerometerFourier",
                    "StandardDeviationXAxisJerkBodyAccelerometerFourier",
                    "StandardDeviationYAxisJerkBodyAccelerometerFourier",
                    "StandardDeviationZAxisJerkBodyAccelerometerFourier",
                    "MeanFrequencyXAxisJerkBodyAccelerometerFourier",
                    "MeanFrequencyYAxisJerkBodyAccelerometerFourier",
                    "MeanFrequencyZAxisJerkBodyAccelerometerFourier",
                    "MeanXAxisBodyGyroscopeFourier",
                    "MeanYAxisBodyGyroscopeFourier",
                    "MeanZAxisBodyGyroscopeFourier",
                    "StandardDeviationXAxisBodyGyroscopeFourier",
                    "StandardDeviationYAxisBodyGyroscopeFourier",
                    "StandardDeviationZAxisBodyGyroscopeFourier",
                    "MeanFrequencyXAxisBodyGyroscopeFourier",
                    "MeanFrequencyYAxisBodyGyroscopeFourier",
                    "MeanFrequencyZAxisBodyGyroscopeFourier",
                    "MeanEuclideanNormBodyAccelerometerFourier",
                    "StandardDeviationEuclideanNormBodyAccelerometerFourier",
                    "MeanFrequencyEuclideanNormBodyAccelerometerFourier",
                    "MeanEuclideanNormJerkBodyAccelerometerFourier",
                    "StandardDeviationEuclideanNormJerkBodyAccelerometerFourier",
                    "MeanFrequencyEuclideanNormJerkBodyAccelerometerFourier",
                    "MeanEuclideanNormBodyGyroscopeFourier",
                    "StandardDeviationEuclideanNormBodyGyroscopeFourier",
                    "MeanFrequencyEuclideanNormBodyGyroscopeFourier",
                    "MeanEuclideanNormJerkBodyGyroscopeFourier",
                    "StandardDeviationEuclideanNormJerkBodyGyroscopeFourieR",
                    "MeanFrequencyEuclideanNormJerkBodyGyroscopeFourier",
                    "AngleBetweenMeanBodyAccelerometerAndGravity",
                    "AngleBetweenMeanJerkBodyAccelerometerAndMeanGravity",
                    "AngleBetweenMeanBodyGyroscopeAndMeanGravity",
                    "AngleBetweenMeanJerkBodyGyroscopeAndMeanGravity",
                    "AngleBetweenXAxisAndMeanGravity",
                    "AngleBetweenYAxisAndMeanGravity",
                    "AngleBetweenZAxisAndMeanGravity",
                    "DataType",
                    "SubjectId",
                    "Activity"
                    
                    )

colnames(meanStdData) <- newColumnNames
#####END OF 4 ###
###### 5 ###

### AVERAGE GROUP BY ACTIVIY AND SUBJECT ####
library(reshape2)

meltedData <- melt(meanStdData, id=c("SubjectId","Activity"),measure.vars=c("MeanXAxisBodyAccelerometer",
                                                                        "MeanYAxisBodyAccelerometer",
                                                                        "MeanZAxisBodyAccelerometer",
                                                                        "StandardDeviationXAxisBodyAccelerometer",
                                                                        "StandardDeviationYAxisBodyAccelerometer",
                                                                        "StandardDeviationZAxisBodyAccelerometer",
                                                                        "MeanXAxisGravityAccelerometer",
                                                                        "MeanYAxisGravityAccelerometer",
                                                                        "MeanZAxisGravityAccelerometer",
                                                                        "StandardDeviationXAxisGravityAccelerometer",
                                                                        "StandardDeviationYAxisGravityAccelerometer",
                                                                        "StandardDeviationZAxisGravityAccelerometer",
                                                                        "MeanXAxisJerkBodyAccelerometer",
                                                                        "MeanYAxisJerkBodyAccelerometer",
                                                                        "MeanZAxisJerkBodyAccelerometer",
                                                                        "StandardDeviationXAxisJerkBodyAccelerometer",
                                                                        "StandardDeviationYAxisJerkBodyAccelerometer",
                                                                        "StandardDeviationZAxisJerkBodyAccelerometer",
                                                                        "MeanXAxisBodyGyroscope",
                                                                        "MeanYAxisBodyGyroscope",
                                                                        "MeanZAxisBodyGyroscope",
                                                                        "StandardDeviationXAxisBodyGyroscope",
                                                                        "StandardDeviationYAxisBodyGyroscope",
                                                                        "StandardDeviationZAxisBodyGyroscope",
                                                                        "MeanXAxisJerkBodyGyroscope",
                                                                        "MeanYAxisJerkBodyGyroscope",
                                                                        "MeanZAxisJerkBodyGyroscope",
                                                                        "StandardDeviationXAxisJerkBodyGyroscope",
                                                                        "StandardDeviationYAxisJerkBodyGyroscope",
                                                                        "StandardDeviationZAxisJerkBodyGyroscope",
                                                                        "MeanEuclideanNormBodyAccelerometer",
                                                                        "StandardDeviationEuclideanNormBodyAccelerometer",
                                                                        "MeanEuclideanNormGravityAccelerometer",
                                                                        "StandardDeviationEuclideanNormGravityAccelerometer",
                                                                        "MeanEuclideanNormJerkBodyAccelerometer",
                                                                        "StandardDeviationEuclideanNormJerkBodyAccelerometer",
                                                                        "MeanEuclideanNormBodyGyroscope",
                                                                        "StandardDeviationEuclideanNormBodyGyroscope",
                                                                        "MeanEuclideanNormJerkBodyGyroscope",
                                                                        "StandardDeviationEuclideanNormJerkBodyGyroscope",
                                                                        "MeanXAxisBodyAccelerometerFourier",
                                                                        "MeanYAxisBodyAccelerometerFourier",
                                                                        "MeanZAxisBodyAccelerometerFourier",
                                                                        "StandardDeviationXAxisBodyAccelerometerFourier",
                                                                        "StandardDeviationYAxisBodyAccelerometerFourier",
                                                                        "StandardDeviationZAxisBodyAccelerometerFourier",
                                                                        "MeanFrequencyXAxisBodyAccelerometerFourier",
                                                                        "MeanFrequencyYAxisBodyAccelerometerFourier",
                                                                        "MeanFrequencyZAxisBodyAccelerometerFourier",
                                                                        "MeanXAxisJerkBodyAccelerometerFourier",
                                                                        "MeanYAxisJerkBodyAccelerometerFourier",
                                                                        "MeanZAxisJerkBodyAccelerometerFourier",
                                                                        "StandardDeviationXAxisJerkBodyAccelerometerFourier",
                                                                        "StandardDeviationYAxisJerkBodyAccelerometerFourier",
                                                                        "StandardDeviationZAxisJerkBodyAccelerometerFourier",
                                                                        "MeanFrequencyXAxisJerkBodyAccelerometerFourier",
                                                                        "MeanFrequencyYAxisJerkBodyAccelerometerFourier",
                                                                        "MeanFrequencyZAxisJerkBodyAccelerometerFourier",
                                                                        "MeanXAxisBodyGyroscopeFourier",
                                                                        "MeanYAxisBodyGyroscopeFourier",
                                                                        "MeanZAxisBodyGyroscopeFourier",
                                                                        "StandardDeviationXAxisBodyGyroscopeFourier",
                                                                        "StandardDeviationYAxisBodyGyroscopeFourier",
                                                                        "StandardDeviationZAxisBodyGyroscopeFourier",
                                                                        "MeanFrequencyXAxisBodyGyroscopeFourier",
                                                                        "MeanFrequencyYAxisBodyGyroscopeFourier",
                                                                        "MeanFrequencyZAxisBodyGyroscopeFourier",
                                                                        "MeanEuclideanNormBodyAccelerometerFourier",
                                                                        "StandardDeviationEuclideanNormBodyAccelerometerFourier",
                                                                        "MeanFrequencyEuclideanNormBodyAccelerometerFourier",
                                                                        "MeanEuclideanNormJerkBodyAccelerometerFourier",
                                                                        "StandardDeviationEuclideanNormJerkBodyAccelerometerFourier",
                                                                        "MeanFrequencyEuclideanNormJerkBodyAccelerometerFourier",
                                                                        "MeanEuclideanNormBodyGyroscopeFourier",
                                                                        "StandardDeviationEuclideanNormBodyGyroscopeFourier",
                                                                        "MeanFrequencyEuclideanNormBodyGyroscopeFourier",
                                                                        "MeanEuclideanNormJerkBodyGyroscopeFourier",
                                                                        "StandardDeviationEuclideanNormJerkBodyGyroscopeFourieR",
                                                                        "MeanFrequencyEuclideanNormJerkBodyGyroscopeFourier",
                                                                        "AngleBetweenMeanBodyAccelerometerAndGravity",
                                                                        "AngleBetweenMeanJerkBodyAccelerometerAndMeanGravity",
                                                                        "AngleBetweenMeanBodyGyroscopeAndMeanGravity",
                                                                        "AngleBetweenMeanJerkBodyGyroscopeAndMeanGravity",
                                                                        "AngleBetweenXAxisAndMeanGravity",
                                                                        "AngleBetweenYAxisAndMeanGravity",
                                                                        "AngleBetweenZAxisAndMeanGravity"))
#calculate means
finalMeanData <- dcast(meltedData, SubjectId + Activity ~ variable, mean)

#export/write dataframe
write.table(finalMeanData,file='./fegoulart.txt',row.name=FALSE)


````
