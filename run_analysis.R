rm(list=ls())
# Merging the training and the test sets to create one data set.
trainD <- read.table("X_train.txt")

head(trainD)

trainL <- read.table("y_train.txt")

trainSub <- read.table("subject_train.txt")
testD <- read.table("X_test.txt")

testL <- read.table("y_test.txt") 

testSub <- read.table("subject_test.txt")

comD <- rbind(trainD, testD)

comL <- rbind(trainL, testL)

comSub <- rbind(trainSub, testSub)


# Extracting only the measurements on the mean and std dev

feat <- read.table("features.txt")

m_stdev_ind <- grep("mean\\(\\)|std\\(\\)", feat[, 2])

comD <- comD[, m_stdev_ind]

names(comD) <- gsub("\\(\\)", "", features[m_stdev_ind, 2])
names(comD) <- gsub("mean", "Mean", names(comD)) 
names(comD) <- gsub("std", "Std", names(comD)) 
names(comD) <- gsub("-", "", names(comD)) 

# Using descriptive activity names to name the activities in the data set

act <- read.table("activity_labels.txt")
act[, 2] <- tolower(gsub("_", "", act[, 2]))
substr(act[2, 2], 8, 8) <- toupper(substr(act[2, 2], 8, 8))
substr(act[3, 2], 8, 8) <- toupper(substr(act[3, 2], 8, 8))
actL <- act[comL[, 1], 2]
comL[, 1] <- actL
names(comL) <-"activity"

# Appropriately labels the data set with descriptive activity names. 
names(comSub) <- "subject"
res_Data <- cbind(comSub, comL, comD)

write.table(res_Data, "merged.txt") 

# Creating a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subLen <- length(table(comSub)) 
actLen <- dim(act)[1]
coLen <- dim(res_Data)[2]

rslt <- matrix(NA, nrow=subLen*actLen, ncol=coLen) 
rslt <- as.data.frame(rslt)
colnames(rslt) <- colnames(res_Data)
row <- 1
for(i in 1:subLen) {
  for(j in 1:actLen) {
    rslt[row, 1] <- sort(unique(comSub)[, 1])[i]
    rslt[row, 2] <- act[j, 2]
    bool1 <- i == res_Data$subject
    bool2 <- act[j, 2] == res_Data$activity
    rslt[row, 3:coLen] <- colMeans(res_Data[bool1&bool2, 3:coLen])
    row <- row + 1
  }
}
head(rslt)
write.table(rslt, "means.txt")


