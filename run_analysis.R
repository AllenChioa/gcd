##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement.
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names
##5.From the data set in step 4, creates a second, independent tidy data set with
##  the average of each variable for each activity and each subject.

run_analysis<-function(){
  
  if (!file.exists("./UCI HAR Dataset")) (
    stop("Pls unzip the dataset zipfile under the current WD.")    
    )
  else{
    ##get the variance names from the features file
    fvar<-read.table("./UCI HAR Dataset/features.txt",header = F)
    ##1.1 Merges the training x and y by cbind.
    dtrainx<-read.table("./UCI HAR Dataset/train/X_train.txt",header = F)
    colnames(dtrainx)<-fvar[,2] # change the variance names using the features
    dtrainy<-read.table("./UCI HAR Dataset/train/y_train.txt",header = F)
    colnames(dtrainy)<-c("activity")# change the variance names  to activity
    dtrainsbj<-read.table("./UCI HAR Dataset/train/subject_train.txt",header = F)
    colnames(dtrainsbj)<-c("subject")# change the variance names  to subject
    dtrain<-cbind(dtrainx,dtrainy,dtrainsbj)
    ##1.2 Merges the test x and y by cbind.
    dtestx<-read.table("./UCI HAR Dataset/test/X_test.txt",header = F)
    colnames(dtestx)<-fvar[,2] # change the variance names using the features
    dtesty<-read.table("./UCI HAR Dataset/test/y_test.txt",header = F)
    colnames(dtesty)<-c("activity")# change the variance names  to activity
    dtestsbj<-read.table("./UCI HAR Dataset/test/subject_test.txt",header = F)
    colnames(dtestsbj)<-c("subject")# change the variance names  to subject
    dtest<-cbind(dtestx,dtesty,dtestsbj)
    ##1.3 Merges the train and test into rows
    dtt<-rbind(dtest,dtrain)
     
    
    ##2.1 filter the colnames use the regular exp: .*[Mm]ean.*|.*[Ss]td.*|subject|activity,
    ##    and store into dtt_ms
    dtt_ms<-dtt[,grep(".*[Mm]ean.*|.*[Ss]td.*|subject|activity",colnames(dtt))]
    
    ##3.1 get the descriptive activity names,
    actlab<-read.table("./UCI HAR Dataset/activity_labels.txt",header = F)
    colnames(actlab)<-c("activityno","activitydesc")    
    ##3.2 using the merger fun, all.x =T, and then delete the origin activity column
    dtt_ms<-merge(dtt_ms,actlab,by.x = "activity",by.y = "activityno",all.x = T,sort = F)
    dtt_ms<-dtt_ms[,-1]
    ##3.2 same way to process the subject , skip here, as no requested in the course
    
    ##4.Appropriately labels the data set with descriptive variable names
    ##  the meaningful column names was given in above steps, skip
    
    ##5.From the data set in step 4, creates a second, independent tidy data set with
    ##  the average of each variable for each activity and each subject. and save to 
    ##  a file named new.txt
    dtt_new<-aggregate.data.frame(dtt_ms[,-(ncol(dtt_ms)-1):-ncol(dtt_ms)],by = list(dtt_ms$subject,dtt_ms$activitydesc),FUN = mean)
    write.table(dtt_new,"./new.txt",row.names = F)
   }

}

