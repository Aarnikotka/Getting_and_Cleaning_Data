#working directory is set to unzipped UCI HAR Dataset

#Step 1: Merge the training and the test sets to create one data set.
#import attribute names
attr_nam <- read.table("features.txt", sep = "")
attr_nam <- attr_nam$V2

#import Xtrain, set attribute names
x_train = read.table("train/X_train.txt", sep = "")
names(x_train) <- attr_nam
#import Ytrain, set attribute names
y_train <- read.table("train/y_train.txt", sep = "")
names(y_train) <- "activity"
#import subject train, set attribute name
train_sub <- read.table("train/subject_train.txt", sep = "")
names(train_sub) <- "subject_id"
#final train set
train <- cbind(train_sub, y_train, x_train)

#import Xtest, set attribute names
x_test <- read.table("test/X_test.txt", sep = "")
names(x_test) <- attr_nam
#import Ytest, set attribute names
y_test <- read.table("test/y_test.txt", sep = "")
names(y_test) <- "activity"
#import subject test, set attribute name
test_sub <- read.table("test/subject_test.txt", sep = "")
names(test_sub) <- "subject_id"
#final test set
test <- cbind(test_sub, y_test, x_test)

#create a united data set, remove temporary variables
united_set<-rbind(test,train)
rm(test,test_sub,train,train_sub,x_test,x_train,y_test,y_train,attr_nam)


#Step 2: Extract only the measurements on the mean and standard deviation for each measurement.
#choose the columns with mean and std (with activity and subject)
cols_mean_std<-c(grep('std',names(united_set)),grep('mean',names(united_set)))
final_set<-united_set[,c(1:2,cols_mean_std)]
#drop columns with meanFreq
final_set<-final_set[,-grep('Freq',names(final_set))]
rm(cols_mean_std)

#Step 3: Use descriptive activity names to name the activities in the data set
#import activity labels
act_lab <- read.table("activity_labels.txt", sep = "")
act_lab <- as.character(act_lab$V2)
#replace numbers in activity column with activity labels
final_set$activity <- as.factor(final_set$activity)
levels(final_set$activity) <- act_lab
rm(act_lab)

#step 4: Appropriately label the data set with descriptive variable names
#complete forms and description are presented in the code book
temp_names<-names(final_set)
temp_names<-gsub('Body','b',temp_names)
temp_names<-gsub('Jerk','j',temp_names)
temp_names<-gsub('Gyro','g',temp_names)
temp_names<-gsub('Acc','a',temp_names)
temp_names<-gsub('Mag','m',temp_names)
temp_names<-gsub('Gravity','r',temp_names)
temp_names<-gsub('mean','m',temp_names)
temp_names<-gsub('std','s',temp_names)
temp_names<-gsub('\\(|\\)','',temp_names)
temp_names<-tolower(temp_names)
temp_names<-sub('subject_','',temp_names)
temp_names<-sub('activity','act',temp_names)
names(final_set)<-temp_names
rm(temp_names)

#Step 5: Independent data set with the average of each variable for each activity and each subject
library(dplyr)
final_av_set<-group_by(final_set,id,act)
final_av_set<-summarize_each(final_av_set,funs(mean))
