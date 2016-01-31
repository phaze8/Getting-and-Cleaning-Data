## Write Comments
run_analysis <- function() {
  ## Gets Data
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile="zipfile.zip", method="curl")
  unzip("zipfile.zip")
  setwd("./UCI HAR Dataset")
  ## Read all data in then combine into one data set
  X_train <- read.table("./train/X_train.txt")
  X_test <- read.table("./test/X_test.txt")
  Y_test <- read.table("./test/Y_test.txt")
  Y_train <- read.table("./train/Y_train.txt")
  subject_test <- read.table("./test/subject_test.txt")
  subject_train <- read.table("./train/subject_train.txt")
  train <- cbind(subject_train, Y_train, X_train)
  test <- cbind(subject_test, Y_test, X_test)
  train_test <- rbind(train, test)
  keep <- c(1,2)  ## Keep first two
  cols <- c("Subject", "Activity")  ## For Column Names
  features <- read.table("./features.txt")
  activities <- read.table("./activity_labels.txt")
  for(i in 1:dim(features)[1]) {  ## Run through each feature looking for any "Mean" matches
    pos <- regexpr("[Mm]ean|[Ss]td", features[i,2])
    if(pos[1] != -1){
      keep <- append(keep, i+2);
      cols <- append(cols, as.character(features[i,2]));
    }
  }
  ## Swaps out Activity Code for Activity Name
  for(i in 1:dim(train_test)[1]){
    train_test[i,2] <- as.character(activities[train_test[i,2],2])
  }
  train_test <- train_test[,keep]
  colnames(train_test) <- cols
  ## Finds Means grouping by Subject and Activity, stores values in a data table
  library(data.table)
  DT <- data.table(train_test)
  DT <- DT[,lapply(.SD, mean, na.rm=TRUE), by=c("Subject", "Activity")]
  both_sets = list(full_data=train_test, summarized_data=DT)
  return(both_sets)
}