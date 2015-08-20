Getting and Cleanning Data Project Assignment Codebook
======================================================

The data used for this script come from the https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip zip file.

*Data frames used in this script:

activity_labels:  Data for this data frame comes from the activity_labels.txt in the zip folder.  The labels are given as follows:  1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING.  This data frame is a 6 by 2 table.


features:  Data for this data frame comes from the features.txt in the zip folder.  These are given as below and form the variable column names.  This data frame is a 561 by 2 table.  The XYZ variables are separated into individual columns:

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

subject_train:  Data for this data frame comes from the subject_train.txt in the zip folder.  They are labeled from 1-30 where each number represent each of the 30 subjects used in the training set.  This is a 7352 by 1 table.

y_train:  Data for this data frame comes from the y_train.txt file in the zip folder, which contains the labels for the different types of activities.  These are labeled from 1-6 for the different activities in the activity_labels.txt file.  This is a 7352 by 1 table.

X_train:  Data for this data frame comes from the X_train.txt file in the zip folder, which contains the data recorded for the different activities.  This is a 7352 by 561 table.

subject_test:  Data for this data frame comes from the subject_test.txt in the zip folder.  They are labeled from 1-30 where each number represent each of the 30 subjects used in the test set.  This is a 2947 by 1 table.

y_test:  Data for this data frame comes from the y_test.txt file in the zip folder, which contains the labels for the different types of activities.  These are labeled from 1-6 for the different activities in the activity_labels.txt file.  This is a 2947 by 1 table.

X_test:  Data for this data frame comes from the X_test.txt file in the zip folder, which contains the data recorded for the different activities.  This is a 2947 by 561 table.

train:  The training set formed by column binding the subject_train and y_train data frames.  Eventually train will be column binded with temp1, temp2 and X_train.  This will be a 7352 by 565 table.

temp1:  A character vector of length equal to the number of row in the train data frame.  This vector will eventually contain the activity descriptions corresponding to the activity labels

temp2:  A character vector of length equal to the number of row in the train data frame.  This vector will be populated with labels of "Training"

test:  The test set formed by column binding the subject_test and y_test data frames.  Eventually test will be column binded with temp3, temp4 and X_test.  This will be a 2947 by 565 table.

temp3:  A character vector of length equal to the number of row in the test data frame.  This vector will eventually contain the activity descriptions corresponding to the activity labels

temp4:  A character vector of length equal to the number of row in the test data frame.  This vector will be populated with labels of "Test"

merged:  This data frame is the row binded combination of the train and test data frames. This data frame will have dimensions of 10299 by 565

mean_std:  This data frame is a subset of the merged data frame with only columns having mean or standard deviation values selected

mean_arranged:  This data frame is a subset of the mean_std data frame with only mean values selected.  Also the data frame is sorted by the Subjects labels and activity labels in ascending order.