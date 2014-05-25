#load data
tsdata<-read.table("test/X_test.txt")
trdata<-read.table("train/X_train.txt")
actkey<-as.matrix(read.table("activity_labels.txt"))
tssub<-as.matrix(read.table("test/subject_test.txt"))
trsub<-as.matrix(read.table("train/subject_train.txt"))
subj<-c(tssub,trsub)
featkey<-as.matrix(read.table("features.txt"))
tsactcol<-read.table("test/y_test.txt")
tractcol<-read.table("train/y_train.txt")
#initialize vectors
tsactfin<-0
tractfin<-0
#add activity labels
for(i in 1:nrow(tsdata)){
  tsactfin[i]<-actkey[tsactcol[i,1],2]
  next
}
for(i in 1:nrow(trdata)){
  tractfin[i]<-actkey[tractcol[i,1],2]
  next
}
#combine two label sets for train and test into one data set
actfin<-c(tsactfin,tractfin)
#list of columns in data set containing labels, means, or standard deviations
meanstdcols<-c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,
  163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,294,295,296,
  345,346,347,348,349,350,373,374,375,424,425,426,427,428,429,452,453,454,503,504,513,516,517,
  526,529,530,539,542,543,552,555,556,557,558,559,560,561)
#take only label, mean, and standard deviation from train and test data sets
tsmeanstd<-subset(tsdata,select=meanstdcols)
trmeanstd<-subset(trdata,select=meanstdcols)
#combine two data sets, train and test, into one data set
meanstd<-rbind(tsmeanstd,trmeanstd)
#add features label
colkey<-0
for(i in 1:86){
  colkey[i]<-featkey[meanstdcols[i],2]
  next
}
#add activity labels and column names
actmeanstd<-cbind(actfin,meanstd)
colnames(actmeanstd)<-c("Activity",colkey)
#write file "tidy.txt"
#was .csv but cannot upload .csv to coursera site
write.table(actmeanstd, file = "tidy.txt", col.names = T)
#create tidy2 data set with average per subject and activity
subactmeanstd<-cbind(subj,actmeanstd)
colnames(subactmeanstd)<-c("Subject","Activity",colkey)
subactavg<-matrix(rep(0,15840),nrow=180,ncol=88)
#scan data for subject and actiovity data, average results, add to subactavg
for(i in 1:30){
  for(j in 1:6){
    ak<-0
    ak<-actkey[j,]
    sb<-subset(subactmeanstd,Activity==ak[2]&Subject==i)
    subactavg[((i-1)*6)+j,1]<-i
    subactavg[((i-1)*6)+j,2]<-actkey[j,2]
    for(k in 3:88){
      mn<-0
      mn<-mean(sb[1:nrow(sb),k])
      subactavg[((i-1)*6)+j,k]<-mn 
      next
    }
    next
  }
  next
}  
#write file "tidy.txt"
#was .csv but cannot upload .csv to coursera site
write.table(subactavg, file = "tidy2.txt", col.names = T)
