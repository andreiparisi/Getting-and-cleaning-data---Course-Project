## Step 1 Merges the training and the test sets to create one data set.
# First lets read all the relevant files

X_test <- read.table("CourseProject/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("CourseProject/UCI HAR Dataset/test/y_test.txt")
X_train <- read.table("CourseProject/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("CourseProject/UCI HAR Dataset/train/y_train.txt")
subject_test <- read.table("CourseProject/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("CourseProject/UCI HAR Dataset/train/subject_train.txt")
features <- read.table("CourseProject/UCI HAR Dataset/features.txt")
activity_labels <- read.table("C:/R working directory/CourseProject/UCI HAR Dataset/activity_labels.txt")
colnames(activity_labels) <- c("activity","activity_label")

# Then the subject and activity info are combined to the measurements for both test and train data
# Also the variable names are assigned according to the "features" file 
X_test <- cbind(y_test, subject_test, X_test)
colnames(X_test) <- c("activity","subject",as.vector(features[ ,2]))
X_train <- cbind(y_train, subject_train, X_train)
colnames(X_train) <- c("activity","subject",as.vector(features[ ,2]))

# Test and train data sets are merged
X <- rbind(X_test,X_train)

## Step 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# The subset is created selecting the relevant columns using the grepl function
X <- X[ , ( grepl( "activity" , names(X)) | grepl( "subject" , names(X)) | grepl( "mean()" , names(X)) | grepl( "std()" , names(X)))  &! grepl( "meanFreq()" , names(X)) ]


## Step 3 Uses descriptive activity names to name the activities in the data set
# The activity labels are assigned to the dataframe doing a left join with the merge function 
X <- merge(activity_labels, X, by="activity", all.y=TRUE)

## Step 4 Appropriately labels the data set with descriptive variable names. 
# The column names are cleaned of the parenthesis and "-" characters because otherwise they need to be escaped 
# when used within functions
colnames(X) <- gsub("\\()","",gsub("-","_", names(X)))

## Step 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# This is done using the "dplyr" package and the functions group_by and summarise each
library(dplyr)
tidy_data <- group_by(X, activity_label, subject)
tidy_data <- summarise_each(tidy_data, funs(mean))

# The tidy data set is written to a text file for submission
write.table(tidy_data ,"CourseProject.txt" , row.name=FALSE)