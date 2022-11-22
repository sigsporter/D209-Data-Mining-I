## Author: Stephen E. Porter
## Title: k-Nearest-Neighbor (Task 1)
## Course: WGU D209: Data Mining I
## Instructor: Dr. Festus Elleh
options(warn=-1)

# Libraries
library(tidyverse)
library(fastDummies)
library(class)
library(caret)
library(pROC)

# Import CSV as data frame
df <- read.csv(file = 'C:/WGU/D209 Data Mining I/churn_clean.csv')

# Checking for nulls
sapply(df, function(x) sum(is.na(x)))
dim(df)


str(df)

# Renaming unclear columns named Item1 through Item8 for improved readability &
# confirming they have been renamed correctly

df <- df %>%
  rename(
    Response = Item1,
    Fix = Item2,
    Replacement = Item3,
    Reliability = Item4,
    Options = Item5,
    Respectful = Item6,
    Courteous = Item7,
    Listening = Item8
  )

colnames(df)

# Several columns will not be useful in analysis and therefore will be dropped.
to_drop <- c('CaseOrder', 'Customer_id', 'Interaction', 'UID', 'City',
             'County', 'Zip', 'Lat', 'Lng', 'TimeZone', 'Job')

dfDropped = df[,!(names(df) %in% to_drop)]
str(dfDropped)

# Creating dummy variables for categorical columns
dfReg <- dummy_cols(dfDropped, remove_selected_columns = TRUE)
names(dfReg) <- gsub(" ", "_", names(dfReg))
names(dfReg) <- gsub("-", "_", names(dfReg))
names(dfReg) <- gsub("[()]", "_", names(dfReg))
str(dfReg)
dim(dfReg)

# Split dfReg into training and testing subsets
set.seed(22)
trainId = createDataPartition(dfReg$Churn_Yes, times = 1, p = 0.7, list = FALSE)

dfTrain = dfReg[trainId,]
dfTest = dfReg[-trainId,]

# Summary Statistics
summary(dfTrain)

# Normalize Function
normalize = function(x) {
  result = (x - min(x)) / (max(x) - min(x))
  return(result)
}

# Normalize training set
dfTrainNorm <- dfTrain
for (i in 1:19) {
  dfTrainNorm[i] <- normalize(dfTrainNorm[i])
}
# Normalize testing set
dfTestNorm <- dfTest

for (i in 1:19) {
  dfTestNorm[i] <- normalize(dfTestNorm[i])
}

# Summary Statistics
summary(dfTrainNorm)

# Export prepared data sets
write.csv(dfTrainNorm, "C:\\WGU\\D209 Data Mining I\\PA Task 1\\D209_dfTrainNorm.csv", row.names = FALSE)
write.csv(dfTestNorm, "C:\\WGU\\D209 Data Mining I\\PA Task 1\\D209_dfTestNorm.csv", row.names = FALSE)

dfTrainChurn <- dfTrainNorm[, -which(names(dfTrainNorm) %in% c("Churn_No"))]
dfTestChurn <- dfTestNorm[, -which(names(dfTestNorm) %in% c("Churn_No"))]

# Finding the best k-value for accuracy

# Initial guess
kVal <- sqrt(nrow(dfTrainChurn)) # kval = 83.666

predFloor <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = floor(kVal)) # k = 83
outcomesFloor <- table(predFloor, dfTestChurn$Churn_Yes)
confFloor <- confusionMatrix(outcomesFloor)
confFloor # Accuracy 0.94

predCeil <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = ceiling(kVal)) # k = 84
outcomesCeil <- table(predCeil, dfTestChurn$Churn_Yes)
confCeil <- confusionMatrix(outcomesCeil)
confCeil # Accuracy 0.939

# Checking other values
pred100 <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = 100)
outcomes100 <- table(pred100, dfTestChurn$Churn_Yes)
conf100 <- confusionMatrix(outcomes100)
conf100 # Accuracy 0.938

pred75 <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = 75)
outcomes75 <- table(pred75, dfTestChurn$Churn_Yes)
conf75 <- confusionMatrix(outcomes75)
conf75 # Accuracy 0.9403

pred50 <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = 50)
outcomes50 <- table(pred50, dfTestChurn$Churn_Yes)
conf50 <- confusionMatrix(outcomes50)
conf50 # Accuracy 0.9513

pred25 <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = 25)
outcomes25 <- table(pred25, dfTestChurn$Churn_Yes)
conf25 <- confusionMatrix(outcomes25)
conf25 # Accuracy 0.948


# k=50 gave the best for a random guess. Checking the 10 on either side of 50.

## The code below was borrowed in part from LearnByMarketing.com
## https://www.learnbymarketing.com/tutorials/k-nearest-neighbors-in-r-example/#:~:text=K-Nearest-Neighbors%20in%20R%20Example%20KNN%20calculates%20the%20distance,class%20%7D%20library%20and%20uses%20the%20knn%20function

churn_acc <- numeric() #Holding variable

# This takes about 2.5-3 minutes to complete.
for(i in 40:60){
  #Apply knn with k = i
  predict <- knn(dfTrainChurn,dfTestChurn,dfTrainChurn$Churn_Yes,k=i)
  churn_acc <- c(churn_acc, mean(predict==dfTestChurn$Churn_Yes))
}

## end third party block
knnAcc <- tibble(x=40:60, y=churn_acc)
knnAcc[which.max(knnAcc$y),]

# k=43 is the best for accuracy. 
pred43 <- knn(train = dfTrainChurn, test = dfTestChurn, cl = dfTrainChurn$Churn_Yes, k = 43, prob = TRUE)
outcomes43 <- table(pred43, dfTestChurn$Churn_Yes)
conf43 <- confusionMatrix(outcomes43)
conf43 # Accuracy 0.9527

pred43Numeric <- as.numeric(as.character(pred43))

rocPlot <- roc(response = dfTestChurn$Churn_Yes, 
                predictor = pred43Numeric,
                percent = TRUE,
                ci=TRUE,
                boot.n=100,
                ci.alpha=-0.9,
                stratified=FALSE,
                plot=TRUE,
                grid=TRUE,
                print.auc=TRUE,
                show.thres=TRUE)
