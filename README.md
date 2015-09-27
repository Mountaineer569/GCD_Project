---
title: "ReadMe"
author: "Mountaineer569"
date: "September 27, 2015"
output: html_document
---

# run_analysis.R
# Getting and Cleaning Data course Project

Instructions
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

 You should create one R script called run_analysis.R that does the following. 
1.	Merges the training and the test sets to create one data set.
2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
3.	Uses descriptive activity names to name the activities in the data set
4.	Appropriately labels the data set with descriptive variable names. 
5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library('data.table')
library('plyr')
library('dplyr')

# After downloading and unzipping the data, we get these files.
```{r}
list.files("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset")
list.files("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train")
list.files("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test")
```
[1] "activity_labels.txt"                         
[3] "features.txt"                  
[5] "features_info.txt"              "README.txt"                    
[1] "Inertial Signals"  "subject_train.txt" "X_train.txt"       "y_train.txt"
[1] "Inertial Signals" "subject_test.txt" "X_test.txt"       "y_test.txt"

Files in the ./Inertial Signals/  folders were not utilized in this project.
See README.txt for a detailed description of the files.
Ultimately, we want to bring together the data sets as shown in this diagram.

Variables|  features.txt |      Subject      | Activity
---------------------------------------------------------------------------
Data     |  X_train.txt  | subject_train.txt | y_train.txt
         | X_test.txt    | subject_test.txt  | Y_test.txt

Where the Activity_labels.txt are mapped to the activity data sets, y_train.txt and y_test.txt.

# Read x data.
```{r}
xtrain <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train/X_train.txt")
dim(xtrain)  # Same number of columns as the other file to be merged?
xtest <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test/X_test.txt")
dim(xtest)  # Same number of columns as the other file to be merged?
```

Read file containing column names of x data and assign to data frames
The make.names function ensures the variable names are valid syntax.
Invalid text is converted to a "." and will be cleaned up later in Step 4.
```{r}
features <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/features.txt")
features <- mutate(features, valid_name = (make.names(features[,2], unique = TRUE)))
colnames(xtrain) <- features[,3]
colnames(xtest) <- features[,3]
```

# Read activity names
activity_labels <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/activity_labels.txt")
str(activity_labels)  # check
# rename column names
```{r}
setnames(activity_labels, "V1","ActivityCode")
setnames(activity_labels, "V2","Activity")
str(activity_labels)  # check
```

# Read y data, which are the activity labels.
```{r}
ytrain <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train/y_train.txt")
str(ytrain)  # Study structure of data frame
# name the y data variable for later use.
setnames(ytrain, "V1","ActivityCode")

ytest <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test/y_test.txt")
str(ytest)  # Study structure of data frame
# name the y data variable for later use.
setnames(ytest, "V1","ActivityCode")
```

# Append the y data (one column) to the x data.
```{r}
dim(xtrain)  # data frame dimensions should increase by on column
xtrain <- cbind(xtrain, ytrain)
dim(xtrain)
dim(xtest)   # data frame dimensions should increase by on column
xtest <- cbind(xtest, ytest)
dim(xtest)
```

# Read subject data.
```{r}
subject_train <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/train/subject_train.txt")
str(subject_train)  # Study structure of data frame
# name the subject data variable for later use.
setnames(subject_train, "V1","Subject")

subject_test <- read.table("C:/Users/Robert/Documents/Coursera/Rprogramming/GCD/UCI HAR Dataset/test/subject_test.txt")
str(subject_test)  # Study structure of data frame
# name the subject data variable for later use.
setnames(subject_test, "V1","Subject")
```

# Append the subject data (one column) to the x data.
```{r}
dim(xtrain)  # data frame dimensions should increase by on column
xtrain <- cbind(xtrain, subject_train)
dim(xtrain)  # check
dim(xtest)   # data frame dimensions should increase by on column
xtest <- cbind(xtest, subject_test)
dim(xtest)   # check
```

# Step 1. Merges the training and the test sets to create one data set.
Combine data frames by rows. 
#################################

Do the dataframes have the same columns so they can be merged by rows?
Result should be all "TRUE"
```{r}
colnames(xtrain) == colnames(xtest)
xdata <- rbind(xtrain, xtest)
dim(xdata)
```

# Step 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
Select colnames that represents mean and std of the data. Combine into one list.
###################################
```{r}
coltarget1 <- grep(c("mean"), features$valid_name, value=T, ignore.case = TRUE)
coltarget2 <- grep(c("std"), features$valid_name, value=T, ignore.case = TRUE)
coltarget <- c(coltarget1, coltarget2, "ActivityCode", "Subject")
xdata2 <- xdata[,coltarget]
```

# Step 3. Use descriptive activity names to name the activities in the data set.
# Activity codes and labels are in the activity_labels data frame.  Map these to xdata2.
##################################
```{r}
xdata2$Activity <- mapvalues(xdata2$ActivityCode, c(1,2,3,4,5,6), 
     c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
# The ActivityCode is no longer needed, so drop it.
set(xdata2, j="ActivityCode", value=NULL)
```

# Step 4. Appropriately labels the data set with descriptive variable names.
The variable names contain many abbreviations, so we need variable names that are more readable to the user.  Also, we need to clean up the variable names from the results of the valid_name function that was run earlier because the valid_name function replaces invalid text with a ".".  Therefore, excess periods may need to be removed from the variable names.  This is simply a search-and-replace task using the gsub function.
.	Replace triple and double periods (".", "..") with single periods (".").
.	Replace prefix "t" with "time".
.	Replace prefix "f" with "FFT", which is Fast Fourier Transformation.
.	Replace "Acc" with "Accerelation".
.	Replace "Mag" with "Magnitude".
.	Replace "Gyr" with "Gyroscope".
###########################################
```{r}
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
```
[1] "timeBodyAccerelation.mean."                   
 [2] "timeBodyAccerelation.mean."                   
 [3] "timeBodyAccerelation.mean."                   
 [4] "timeGravityAccerelation.mean."                
 [5] "timeGravityAccerelation.mean."                
 [6] "timeGravityAccerelation.mean."                
 [7] "timeBodyAccerelationJerk.mean."               
 [8] "timeBodyAccerelationJerk.mean."               
 [9] "timeBodyAccerelationJerk.mean."               
[10] "timeBodyGryoscope.mean."                      
[11] "timeBodyGryoscope.mean."                      
[12] "timeBodyGryoscope.mean."                      
[13] "timeBodyGryoscopeJerk.mean."                  
[14] "timeBodyGryoscopeJerk.mean."                  
[15] "timeBodyGryoscopeJerk.mean."                  
[16] "timeBodyAccerelationMagnitude.mean."          
[17] "timeGravityAccerelationMagnitude.mean."       
[18] "timeBodyAccerelationJerkMagnitude.mean."      
[19] "timeBodyGryoscopeMagnitude.mean."             
[20] "timeBodyGryoscopeJerkMagnitude.mean."         
[21] "FFTBodyAccerelation.mean."                    
[22] "FFTBodyAccerelation.mean."                    
[23] "FFTBodyAccerelation.mean."                    
[24] "FFTBodyAccerelation.mean....X"                
[25] "FFTBodyAccerelation.mean....Y"                
[26] "FFTBodyAccerelation.mean....Z"                
[27] "FFTBodyAccerelationJerk.mean."                
[28] "FFTBodyAccerelationJerk.mean."                
[29] "FFTBodyAccerelationJerk.mean."                
[30] "FFTBodyAccerelationJerk.mean....X"            
[31] "FFTBodyAccerelationJerk.mean....Y"            
[32] "FFTBodyAccerelationJerk.mean....Z"            
[33] "FFTBodyGryoscope.mean."                       
[34] "FFTBodyGryoscope.mean."                       
[35] "FFTBodyGryoscope.mean."                       
[36] "FFTBodyGryoscope.mean....X"                   
[37] "FFTBodyGryoscope.mean....Y"                   
[38] "FFTBodyGryoscope.mean....Z"                   
[39] "FFTBodyAccerelationMagnitude.mean."           
[40] "FFTBodyAccerelationMagnitude.mean..."         
[41] "FFTBodyBodyAccerelationJerkMagnitude.mean."   
[42] "FFTBodyBodyAccerelationJerkMagnitude.mean..." 
[43] "FFTBodyBodyGryoscopeMagnitude.mean."          
[44] "FFTBodyBodyGryoscopeMagnitude.mean..."        
[45] "FFTBodyBodyGryoscopeJerkMagnitude.mean."      
[46] "FFTBodyBodyGryoscopeJerkMagnitude.mean..."    
[47] "angle.tBodyAccerelationMean.gravity."         
[48] "angle.tBodyAccerelationJerkMean..gravityMean."
[49] "angle.tBodyGryoscopeMean.gravityMean."        
[50] "angle.tBodyGryoscopeJerkMean.gravityMean."    
[51] "angle.X.gravityMean."                         
[52] "angle.Y.gravityMean."                         
[53] "angle.Z.gravityMean."                         
[54] "timeBodyAccerelation.std."                    
[55] "timeBodyAccerelation.std."                    
[56] "timeBodyAccerelation.std."                    
[57] "timeGravityAccerelation.std."                 
[58] "timeGravityAccerelation.std."                 
[59] "timeGravityAccerelation.std."                 
[60] "timeBodyAccerelationJerk.std."                
[61] "timeBodyAccerelationJerk.std."                
[62] "timeBodyAccerelationJerk.std."                
[63] "timeBodyGryoscope.std."                       
[64] "timeBodyGryoscope.std."                       
[65] "timeBodyGryoscope.std."                       
[66] "timeBodyGryoscopeJerk.std."                   
[67] "timeBodyGryoscopeJerk.std."                   
[68] "timeBodyGryoscopeJerk.std."                   
[69] "timeBodyAccerelationMagnitude.std."           
[70] "timeGravityAccerelationMagnitude.std."        
[71] "timeBodyAccerelationJerkMagnitude.std."       
[72] "timeBodyGryoscopeMagnitude.std."              
[73] "timeBodyGryoscopeJerkMagnitude.std."          
[74] "FFTBodyAccerelation.std."                     
[75] "FFTBodyAccerelation.std."                     
[76] "FFTBodyAccerelation.std."                     
[77] "FFTBodyAccerelationJerk.std."                 
[78] "FFTBodyAccerelationJerk.std."                 
[79] "FFTBodyAccerelationJerk.std."                 
[80] "FFTBodyGryoscope.std."                        
[81] "FFTBodyGryoscope.std."                        
[82] "FFTBodyGryoscope.std."                        
[83] "FFTBodyAccerelationMagnitude.std."            
[84] "FFTBodyBodyAccerelationJerkMagnitude.std."    
[85] "FFTBodyBodyGryoscopeMagnitude.std."           
[86] "FFTBodyBodyGryoscopeJerkMagnitude.std."       
[87] "Subject"                                      
[88] "Activity"                  

# Step 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
#########################################
```{r}
averages_data <- ddply(xdata2, .(Subject, Activity), function(x) colMeans(x[,1:86]))
write.table(averages_data, "averages_data.txt", row.name=FALSE)
```
