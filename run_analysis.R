# This is an R script for extracting data and creating
# two tidy-based data sets. 

library(sqldf)


# Function to read in the map of value to activity label
# text descriptions. File has the following representation:
#   Value in training/text   Text description
#            1                 LAYING
#            2                 SITTING
#            3                 STANDING
#            4                 WALKING
#            5                 WALKING_DOWNSTAIRS
#            6                 WALKING_UPSTAIRS
#
get_activity_labels <- function(filename)
{
    data <- read.table(filename)
    
    data
}

# Function to read in the feature labels for each column
# in the test and training sets
#
get_feature_labels <- function(filename)
{
    data <- read.table(filename, stringsAsFactors=FALSE)
    
    data
}

# Function to extract the columns with "mean" and "std" in the title of the column
# name. The decision was made to capture all columns that contain "mean" or "std"
# in the name of a column. This decision was based on lack of data (or previous
# documentation) as to the complete meaning of each column. In the interest of 
# being conservative and capturing all data for future use.
#
extract_feature_labels <- function(labels)
{
    labels_to_save <- c()
    labels_to_save <- append(labels_to_save, grep("-mean", feature_labels[,2]))
    labels_to_save <- append(labels_to_save, grep("-std", feature_labels[,2]))
    sort(labels_to_save)
    
    labels_to_save
}

# Function to read in a data set (test or training) along with the necessary
# supporting files for buidling a data frame for return
#
get_data_set <- function(training, 
                         subjects, 
                         activities, 
                         activity_labels,
                         feature_labels)
{
    subject_data <- read.table(subjects, stringsAsFactors=FALSE)
    
    activity_data <- read.table(activities, stringsAsFactors=FALSE)
    activity_data[,1] <- as.factor(activity_data[,1])
    levels(activity_data[,1]) <- activity_labels[,2]
    
    data <- read.table(training, stringsAsFactors=FALSE)
    
    data <- cbind(subject_data, activity_data, data)
    
    names <- c("Subject_Id", "Activity", feature_labels$V2)
    
    names <- gsub("-", "_", names)
    names <- gsub("\\(\\)", "", names)
    #names <- gsub("\)", "", names)
    
    colnames(data) <- names
    
    data
}

# This function builds an sql query using the array of column names as input.
# Used in qierying the combined test/train data set to subselct the output
# into the format required for the second data set.
#
query <- function(column_names)
{
    q <- paste("select ", column_names[1], ",", column_names[2], sep="")
    
    for (i in 3:length(column_names))
    {
        cmd <- paste("Avg(", column_names[i], ")", sep="")
        q <- paste (q, ",", cmd, sep="")
    }
    
    q <- paste(q, "from complete_data_set group by Subject_Id, Activity", sep=" ")
    
    q
}

# Load and and build supporting data structures for formatting the output
# file as described in the project description.
activity_labels <- get_activity_labels("UCI HAR Dataset/activity_labels.txt")
feature_labels <- get_feature_labels("UCI HAR Dataset/features.txt")
columns_to_extract <- extract_feature_labels(feature_labels)
columns_to_extract <- columns_to_extract + 2
columns_to_extract <- c(1, 2, columns_to_extract)

# Load in the training set with all columns
training_set <- get_data_set("UCI HAR Dataset/train/X_train.txt",
                             "UCI HAR Dataset/train/subject_train.txt",
                             "UCI HAR Dataset/train/y_train.txt",
                             activity_labels,
                             feature_labels)

# Extract the required set of columns from the complete training set
training_set <- training_set[, columns_to_extract]

# Load in the test set with all columns
test_set <- get_data_set("UCI HAR Dataset/test/X_test.txt",
                             "UCI HAR Dataset/test/subject_test.txt",
                             "UCI HAR Dataset/test/y_test.txt",
                             activity_labels,
                             feature_labels)

# Extract the required set of columns from the complete test set
test_set <- test_set[, columns_to_extract]

# Combine the training and test set into a complete data set
complete_data_set <- rbind(training_set, test_set)

# Save the complete data set as a comma separated file
write.table(complete_data_set,
            file="UCI HAR Dataset/train_test_mean.txt",
            sep=",",
            row.names=FALSE,
            col.names=TRUE)

# Create the sql query and extract the second data set
average_by_subject_activity <- sqldf(query(colnames(complete_data_set)))

# Save the second data set as a comma separated file
write.table(average_by_subject_activity,
            file="UCI HAR Dataset/average_by_subject_activity.txt",
            sep=",",
            row.names=FALSE,
            col.names=TRUE)
