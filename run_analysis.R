#this function is written to complete the course project of "Getting and Cleaning Data" hosted at Coursera, session Jan 5th - Feb 2nd 2015
#before executing this function, please make sure you have copied samsung data under the working directory. In other words, a directory "UCI HAR Dataset"
#should exist under the workign directory

run_analysis <- function() {
    
    library(plyr)

    #0. read file into memory
    
    X_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
    X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
    
    y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
    y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
    
    subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
    subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
    
    activity_labels <- read.table('./UCI HAR Dataset//activity_labels.txt')
    
    features <- read.table('./UCI HAR Dataset//features.txt')
    
    #1. merge data to a new dataset 
    
    X <- rbind(X_test,X_train)
    Activity <- rbind(y_test,y_train)
    subject <- rbind(subject_test,subject_train)
    
    merged_data <- cbind(Activity, subject)
    colnames(merged_data) <- c("Activity","Subject")
    
    len <- nrow(features)
    f_chr <- as.character(features$V2)
    col_name <- as.character(features$V1)
    f_chr <- gsub("BodyBody","Body",f_chr)
    f_chr <- gsub("\\(\\)","",f_chr)
    f_chr <- gsub("\\(","_",f_chr)
    f_chr <- gsub("\\)","_",f_chr)
    f_chr <- gsub("\\-","_",f_chr)
    
    #2. extract only std and mean measurements to the new dataset
    
    for (i in 1:len) {
        if (length(grep('mean',f_chr[i],ignore.case=TRUE)>0) || length(grep('std',f_chr[i],ignore.case=TRUE))>0) {
            name <- paste('V',col_name[i],sep="")
            merged_data[f_chr[i]] = X[name] 
        }
    }
    
    
    #3. use descriptive activity names to name activities in the dataset
    
    len <- nrow(merged_data)
    for (i in 1:len){
        merged_data[i,1] <- as.character(activity_labels[merged_data[i,1],2])
    }
    
    #4. variable names are actually appropriately assigned in step 1-3
    
    print("Merged data")
    print(names(merged_data))
        
    
    #5. create a second, independent tidy dataset with the average of each variable for each activity and each subject
    
    new_data <- ddply(merged_data, .(Activity,Subject), numcolwise(mean))
    
    #write.table(data,'./newdata.txt',row.names=FALSE)
    
    new_data


}