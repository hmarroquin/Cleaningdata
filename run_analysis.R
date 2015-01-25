## hmarroquin
## 2015-01-24


##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names. 
##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##Test Data
        xtestdata <- read.table(paste(getwd(),"/UCI HAR Dataset/test/X_test.txt",sep=""), head = FALSE)
        ytestdata <- read.table(paste(getwd(),"/UCI HAR Dataset/test/Y_test.txt",sep=""), head = FALSE)
        testxsubjectdata <- read.table(paste(getwd(),"/UCI HAR Dataset/test/subject_test.txt",sep=""), head = FALSE)
##Training Data
        xtraindata <- read.table(paste(getwd(),"/UCI HAR Dataset/train/X_train.txt",sep=""), head = FALSE)
        ytraindata <- read.table(paste(getwd(),"/UCI HAR Dataset/train/Y_train.txt",sep=""), head = FALSE)
        trainysubjectdata <- read.table(paste(getwd(),"/UCI HAR Dataset/train/subject_train.txt",sep=""), head = FALSE)
        
##Features and activity labels
        featuresdata <- read.table(paste(getwd(),"/UCI HAR Dataset/features.txt",sep=""), head = FALSE)
        activitylabels <- read.table(paste(getwd(),"/UCI HAR Dataset/activity_labels.txt",sep=""),header = FALSE)
        
##Merge individual data sets
        ydata <- rbind(ytestdata, ytraindata)
        xdata <- rbind(xtestdata, xtraindata)
        subjectdata <- rbind(testxsubjectdata, trainysubjectdata)
        
## Name Variables/Obs
        names(ydata) <- c("activity")
        names(xdata) <- featuresdata$V2
        names(subjectdata) <- c("name")
        
## Bring it all togethers
        nameandactivitycombined <- cbind(subjectdata, ydata)
        FullData <-cbind(nameandactivitycombined,xdata)
        



##Obtain Mean and STD labels.
        ## double // used to include brackets in syntax
        selectfeaturesdata <- featuresdata$V2[grep("mean\\(\\)|std\\(\\)",featuresdata$V2)]
##construct select criteria for full data frame
        selectcriteria <- c(as.character(selectfeaturesdata),"name","activity")
## subset data with selectedcriteria
        FullDataComplete <- subset(FullData,select = selectcriteria)
        
## Assign activity lables
        FullDataComplete$activity <- factor(FullDataComplete$activity, labels = activitylabels$V2)
        

## Clean labels further        
        
        names(FullDataComplete)<-gsub("^t", "Time", names(FullDataComplete))
        names(FullDataComplete)<-gsub("^f", "frequency", names(FullDataComplete))
        names(FullDataComplete)<-gsub("Acc", "Accelerometer", names(FullDataComplete))
        names(FullDataComplete)<-gsub("Gyro", "Gyroscope", names(FullDataComplete))
        names(FullDataComplete)<-gsub("Mag", "Magnitude", names(FullDataComplete))
        names(FullDataComplete)<-gsub("BodyBody", "Body", names(FullDataComplete))
        

## create tidy data set
        
        library(plyr);
        tidydata<-aggregate(. ~name + activity, FullDataComplete, mean)
        tidydata<-tidydata[order(tidydata$name,tidydata$activity),]
        write.table(tidydata, file = "tidydata.txt",row.name=FALSE)

   
