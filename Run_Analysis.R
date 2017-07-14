
#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

#Reading in other variables
setwd("~/Desktop/Coursera/Course 3/Week 4/UCI HAR Dataset")
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

#Reading in Test Data
setwd("~/Desktop/Coursera/Course 3/Week 4/UCI HAR Dataset/test")
X_test <- read.table("X_test.txt")
Y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

#Reading in Training Data
setwd("~/Desktop/Coursera/Course 3/Week 4/UCI HAR Dataset/train")
X_train <- read.table("X_train.txt")
Y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")



#Name the 561 columns of X_test after the features data
colnames(X_test) <- features[,2]

#Combine the X_test data, with the subject_test information so we know which measurements correspond to which subjects
X_test <- cbind(subject_test, X_test)

#Rename the Subject column to "Subject_test"
colnames(X_test)[1] <- "Subject_test"

#Combine the X_test data with the Y_test data so we can see which activity was being performed during each measurement
X_test <- cbind(Y_test, X_test)

#Rename the Y_test column to "Activity"
colnames(X_test)[1] <- "Activity_labels"


#Let's try merging the activity_labels and Y_test data files

colnames(activity_labels)[1] <- "Activity_label"
colnames(Y_test)[1] <- "Activity_label"
library(plyr)
test <- join(Y_test, activity_labels, by = "Activity_label")
colnames(test)[2] <- "Activity"
test <- test[,2]
test <- cbind(test, X_test)
colnames(test)[1] <- "Activity"


#Finding columns with mean and standard deviation
meancolumns <- grep("mean", features$V2)
stdcolumns <- grep("std", features$V2)
keepcols <- c(meancolumns, stdcolumns)
keepcols <- keepcols + 3

cleaned_test_data <- test[, c(1, 2, 3, keepcols)]



#Now lets do the same with the training dataset.

#Name the 561 columns of X_train after the features data
colnames(X_train) <- features[,2]

#Combine the X_test data, with the subject_test information so we know which measurements correspond to which subjects
X_train <- cbind(subject_train, X_train)

#Rename the Subject column to "Subject_train"
colnames(X_train)[1] <- "Subject_train"

#Combine the X_train data with the Y_train data so we can see which activity was being performed during each measurement
X_train <- cbind(Y_train, X_train)

#Rename the Y_test column to "Activity"
colnames(X_train)[1] <- "Activity_labels"


#Let's try merging the activity_labels and Y_test data files

colnames(Y_train)[1] <- "Activity_label"
train <- join(Y_train, activity_labels, by = "Activity_label")
colnames(train)[2] <- "Activity"
train <- train[,2]
train <- cbind(train, X_train)
colnames(train)[1] <- "Activity"


#Finding columns with mean and standard deviation
meancolumns <- grep("mean", features$V2)
stdcolumns <- grep("std", features$V2)
keepcols <- c(meancolumns, stdcolumns)
keepcols <- keepcols + 3

#Clean up my dataframe
cleaned_train_data <- train[, c(1, 2, 3, keepcols)]

#Change column names so they are uniform
colnames(cleaned_test_data)[3] <- "Subject"
colnames(cleaned_train_data)[3] <- "Subject"

#Add a column that defines the test data AS test or train data AS train
Test_def <- rep("TestSet", length = 2947)
Train_def <- rep("TrainSet", length = 7352)
cleaned_test_data <- cbind(Test_def, cleaned_test_data)
cleaned_train_data <- cbind(Train_def, cleaned_train_data)

#Clean up dataframe a bit
cleaned_test_data$Activity_labels <- NULL
cleaned_train_data$Activity_labels <- NULL
colnames(cleaned_test_data)[1] <- "Test_or_Training_dataset"
colnames(cleaned_train_data)[1] <- "Test_or_Training_dataset"

#combine test and train data into one nice, tidy dataframe
total_cleaned_data <- rbind(cleaned_test_data, cleaned_train_data)

#Now let's find the mean of our variables in each of our subjects, and the activities they are doing!
library(reshape2)

#Let's reshape our data into the long format
meandata <- melt(total_cleaned_data, id.vars = c("Subject", "Activity"))
meandata <- meandata[10300:823920, ]

#Now let's calculate group means
meandata <- transform(meandata, value = as.numeric(value))
meandata_test <- aggregate(value~Subject+Activity+variable, data = meandata, FUN = mean)
colnames(meandata_test)[4] <- "Mean"


#Write data out to a file
write.table(meandata_test, file = "./meandata.txt", row.names = FALSE)






