# run_analysis.R
# created 27-Sep-2015
# Getting and Cleaning Data course Project

library('data.table')
library('plyr')
library('dplyr')

# After downloading and unzipping the data, we get these files.
list.files("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset")
list.files("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train")
list.files("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test")

# Read x data.
xtrain <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train/X_train.txt")
dim(xtrain)  # Same number of columns as the other file to be merged?
xtest <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test/X_test.txt")
dim(xtest)  # Same number of columns as the other file to be merged?

# Read file containing column names of x data and assign to data frames
# The make.names function ensures the variable names are valid syntax.
# Invalid text is converted to a "." and will be cleaned up later in Step 4.
features <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/features.txt")
features <- mutate(features, valid_name = (make.names(features[,2], unique = TRUE)))
colnames(xtrain) <- features[,3]
colnames(xtest) <- features[,3]

# Read activity names
activity_labels <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/activity_labels.txt")
str(activity_labels)  # check
# rename column names
setnames(activity_labels, "V1","ActivityCode")
setnames(activity_labels, "V2","Activity")
str(activity_labels)  # check

# Read y data, which are the activity labels.
ytrain <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train/y_train.txt")
str(ytrain)  # Study structure of data frame
# name the y data variable for later use.
setnames(ytrain, "V1","ActivityCode")

ytest <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test/y_test.txt")
str(ytest)  # Study structure of data frame
# name the y data variable for later use.
setnames(ytest, "V1","ActivityCode")

# Append the y data (one column) to the x data.
dim(xtrain)  # data frame dimensions should increase by on column
xtrain <- cbind(xtrain, ytrain)
dim(xtrain)
dim(xtest)   # data frame dimensions should increase by on column
xtest <- cbind(xtest, ytest)
dim(xtest)

# Read subject data.
subject_train <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train/subject_train.txt")
str(subject_train)  # Study structure of data frame
# name the subject data variable for later use.
setnames(subject_train, "V1","Subject")

subject_test <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test/subject_test.txt")
str(subject_test)  # Study structure of data frame
# name the subject data variable for later use.
setnames(subject_test, "V1","Subject")


# Append the subject data (one column) to the x data.
dim(xtrain)  # data frame dimensions should increase by on column
xtrain <- cbind(xtrain, subject_train)
dim(xtrain)  # check
dim(xtest)   # data frame dimensions should increase by on column
xtest <- cbind(xtest, subject_test)
dim(xtest)   # check


# Step 1. Merges the training and the test sets to create one data set.
# Combine data frames by rows.  This completes Step 1.
#################################

# Do the dataframes have the same columns so they can be merged by rows?
# Result should be all "TRUE"
colnames(xtrain) == colnames(xtest)

xdata <- rbind(xtrain, xtest)
dim(xdata)


# Step 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Select colnames that represents mean and std of the data. Combine into one list.
###################################

coltarget1 <- grep(c("mean"), features$valid_name, value=T, ignore.case = TRUE)
coltarget2 <- grep(c("std"), features$valid_name, value=T, ignore.case = TRUE)
coltarget <- c(coltarget1, coltarget2, "ActivityCode", "Subject")

# Select targeted columns and all rows from xdata.
# This completes step 2.
xdata2 <- xdata[,coltarget]

# Step 3. Use descriptive activity names to name the activities in the data set.
# Activity codes and labels are in the activity_labels data frame.  Map these to xdata2.
##################################

xdata2$Activity <- mapvalues(xdata2$ActivityCode, c(1,2,3,4,5,6), 
     c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
# The ActivityCode is no longer needed, so drop it.
set(xdata2, j="ActivityCode", value=NULL)

# Step 4. Appropriately labels the data set with descriptive variable names.
# Also, clean up the variable names from the results of the valid_name function ran earlier. 
# The valid_name function replaces invalid text with a "." so excess periods may need to be 
# removed from the variable names.
# Now replace all matches of a string.
###########################################

names(xdata2) <- gsub("mean...","mean.", names(xdata2))
names(xdata2) <- gsub("mean..","mean.", names(xdata2))
names(xdata2) <- gsub("mean.q...","mean.q.", names(xdata2))
names(xdata2) <- gsub("mean.q..","mean.q.", names(xdata2))
names(xdata2) <- gsub("std...","std.", names(xdata2))
names(xdata2) <- gsub("std..","std.", names(xdata2))
names(xdata2) <- gsub("^t","time", names(xdata2))
names(xdata2) <- gsub("^f","FFT", names(xdata2))
names(xdata2) <- gsub("Acc","Accerelation", names(xdata2))
names(xdata2) <- gsub("Mag","Magnitude", names(xdata2))
names(xdata2) <- gsub("Gyro","Gryoscope", names(xdata2))
# Check that the colnames look readable.
colnames(xdata2)


# Step 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
#########################################

averages_data <- ddply(xdata2, .(Subject, Activity), function(x) colMeans(x[,1:86]))
write.table(averages_data, "averages_data.txt", row.name=FALSE)

