#########################################################################################

# run_analysis.R - file description
# See also readme and codebook files in same repository.
# 
#This script will take data from accelerometers of the Samsung Galaxy S smartphone
# (UCI HAR Dataset) obtained from:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
# This data, organized in a test and a train version, will be loaded, cleaned, labeled
# and merged. From the merged data frame a new file will be created containing the
# average of each variable for each activity and each subject.
# This script requires the dplyr package. if not already installed, it will download and
# load the packages from CRAN.

#########################################################################################

# Check for and load dplyr package
if (!require("dplyr")) {
        install.packages("dplyr")
}
require(dplyr)

# reading in column names and activity labels:
features <-  read.table("features.txt",sep=" ",header=FALSE)
labels <- read.table("activity_labels.txt",sep="",header=FALSE)

# Reading in test data, test subject vector and activity vector:
x_Test <- read.table("./test/X_test.txt",sep="",header=FALSE)
subject_test <- read.table("./test/subject_test.txt",sep="",header=FALSE)
ytest <- read.table("./test/y_test.txt",sep="",header=FALSE)


# Reading in train data, train subject vector and activity vector:
subject_train <- read.table("./train/subject_train.txt",sep="",header=FALSE)
x_Train <- read.table("./train/X_train.txt" ,  sep="",header=FALSE)
ytrain <- read.table("./train/y_train.txt",sep="",header=FALSE)

# creating a combined vector for column names:
colNames <- c("Subject","Activity",as.character(features$V2))

# cleaning the variable names to increase readability:                          (TASK 4)
for (i in 1:length(colNames)){
        colNames[i] <- gsub("^t","Time",colNames[i])
        colNames[i] <- gsub("^f","Frequency",colNames[i])
        colNames[i] <- gsub("Acc","Acceleration",colNames[i])
        colNames[i] <- gsub("Mag","Magnitude",colNames[i])
        colNames[i] <- gsub("AccelerationJerk","Jerk",colNames[i])
        colNames[i] <- gsub("BodyBody","Body",colNames[i])
        colNames[i] <- gsub("bandsEnergy","EnergyOfInterval",colNames[i])
        colNames[i] <- gsub("std","StdDev",colNames[i])
        colNames[i] <- gsub("mean","Mean",colNames[i])
        colNames[i] <- gsub("\\(\\)","",colNames[i])
}

# combining all test and all train data:
Test  <- cbind(subject_test,ytest,x_Test)
Train <- cbind(subject_train,ytrain,x_Train)

# naming columns in test and train data:
colnames(Train) <- colNames
colnames(Test)  <- colNames

# creating a logical vector to subset both data frames:
# selecting the column names containing "mean" and "standard deviation"
# 1,2 are added for variables "Subject" and "Activity"                          (TASK 2)
logical1 <- c(1,2,grep("mean|std",features$V2)+2)
Train <- Train[,logical1]
Test <- Test[,logical1]

# binding test and train dataframe by rows:                                     (TASK 1)
data <- rbind(Train,Test)

# creating describtive labels for the activity variable:                        (TASK 3)
data$Activity <- factor(data$Activity)
levels(data$Activity) <- labels$V2


# create final dataframe containing the average value of each variable 
# for each activity and each subject:                                           (TASK 5)

finaldata <- aggregate(data[names(data[,3:81])], by=data[c("Subject","Activity")], FUN=mean)

# add "Mean of" to varaible names:
colnam <- names(finaldata)

for ( i in 3:length(colnam)){
        colnam[i] <- paste("Mean of ",colnam[i],sep="")
}

colnames(finaldata) <- colnam

# write out data frame as a .txt file, one white space used as column seperator:
# file created: finalData.txt in current directory:
write.table(finaldata, "./finalData.txt",row.names=TRUE,sep=" ")





