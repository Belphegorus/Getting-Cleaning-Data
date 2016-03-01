library(plyr)
library(greport)

rm(list=ls())



#Loading Data
  
  trainD <- read.table("X_train.txt")
  
  trainL <- read.table("y_train.txt")
  
  trainSub <- read.table("subject_train.txt")
  
  testD <- read.table("X_test.txt")
  
  testL <- read.table("y_test.txt") 
  
  testSub <- read.table("subject_test.txt")
  
  
#Preparations

  feat <- read.table("features.txt",col.names=c("featID", "fLabel"))

  mean_stdev <- grep("-mean\\(\\)|-std\\(\\)", feat$fLabel)  
  
  acts <- read.table("activity_labels.txt", col.names=c("actID", "aLabel"))
  acts$aLabel <- gsub("_", "", as.character(acts$aLabel))
 
  
# Merging datasets
  comSub <- rbind(testSub, trainSub)
  names(comSub) <- "Id"
  comD <- rbind(testD, trainD)
  comD <- comD[, mean_stdev]
  names(comD) <- gsub("\\(|\\)", "", feat$fLabel[mean_stdev])
  comL<- rbind(testL, trainL)
  names(comL) = "actID"
  act <- merge(comL, acts, by="actID")$aLabel
  
# Merging data frames 
  tab<- cbind(comSub, comD, act)
  write.table(tab, "tidy_data.txt")
  
  # create a dataset grouped by subject and activity after applying standard deviation and average calculations
  library(data.table)
  DT <- data.table(tab)
  cDT<- DT[, lapply(.SD, mean), by=c("Id", "activity")]
  write.table(cDT, "mean.txt")