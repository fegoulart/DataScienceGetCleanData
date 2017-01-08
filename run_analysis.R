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
write.table(finalMeanData,file='./fegoulart.txt')


