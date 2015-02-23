# this script processes wearable device data
# it should be executed with the "UCI HAR Dataset" as a subfolder 
# to the current working directory

# start by reading the test and train data files
testData <- read.table("UCI HAR Dataset/test/X_test.txt")
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")

# read also the corresponding activity labels, first for test
yfileTest <- file("UCI HAR Dataset/test/y_test.txt")
labelsTest <- readLines(yfileTest)
close(yfileTest)

# add the labels as a new column in the test data frame
testData <- cbind(testData, label = labelsTest)

# now do the same for the training data, first read the labels...
yfileTrain <- file("UCI HAR Dataset/train/y_train.txt")
labelsTrain <- readLines(yfileTrain)
close(yfileTrain)

# then bind them as a new column in the training data frame
trainData <- cbind(trainData, label = labelsTrain)

# read also subject data from file, for test...
subjFileTest <- file("UCI HAR Dataset/test/subject_test.txt")
subjTest <- readLines(subjFileTest)
close(subjFileTest)

# bind the subjects onto the test data frame, as a new column
testData <- cbind(testData, Subject = subjTest)

# same again with training data, read subjects from file...
subjFileTrain <- file("UCI HAR Dataset/train/subject_train.txt")
subjTrain <- readLines(subjFileTrain)
close(subjFileTrain)

# bind the subjects onto the training data frame
trainData <- cbind(trainData, Subject = subjTrain)


# add the test and train data together into a single data frame
allData <- rbind(testData, trainData)

# read in the feature definition list from file
f <- file("UCI HAR Dataset/features.txt")
features <- readLines(f)
close(f)

featureNames <- character()
# format the feature list into a character vector with all names in order
# first split the whole vector, then loop over elements to extract labels
splFeatures <- strsplit(features, ' ')
for (i in seq(length(splFeatures))) {
    featureNames <- c(featureNames, splFeatures[[i]][2])
}

# give the columns in allData proper, explanatory feature names
# we've maintained order so featureNames holds the first 561 names
# we just need to add a label for our previously added activity/label column
names(allData) <- c(featureNames, "Activity", "Subject")

# we're only interested in mean and standard deviation information, so...
# find the column names that contain "mean()" or "std()" and keep only those
# OK, we naturally keep our Activity and Subject columns too...
allData <- allData[ , grepl("mean\\(\\)", names(allData)) | 
                        grepl("std\\(\\)", names(allData)) | 
                        grepl("Activity", names(allData)) |
                        grepl("Subject", names(allData))]

# now open and read the activity index definitions from file
labelFile <- file("UCI HAR Dataset/activity_labels.txt")
actLabels <- readLines(labelFile)
close(labelFile)

# coerce the activity column to character class, preparing it for changes
allData$Activity <- as.character(allData$Activity)

# for each type of activity, replace the index in the activity column (1 etc)
# by the corresponding text label (WALKING etc)
splActLabels <- strsplit(actLabels, " ")
for (i in seq(6)) {
    actId <- splActLabels[[i]][1] # slightly unnecessary... 
    actName <- splActLabels[[i]][2]
    allData$Activity[allData$Activity == actId] <- actName
}

# allData now contains a nicely structured data set (up until point 4)
# aggregate the data to a tidy data set with the mean of each variable
# the variables are found in allData's columns 1:66; 67 and 68 are activity/subject
tidySet <- aggregate(allData[ , 1:66], by=list(allData$Activity, 
                                allData$Subject), FUN=mean, na.rm=TRUE)

# fix up the titles in the tidy data set
names(tidySet)[1] <- "Activity"
names(tidySet)[2] <- "Subject"

# write to file
write.table(tidySet, file="tidy_set.txt", row.name=FALSE)