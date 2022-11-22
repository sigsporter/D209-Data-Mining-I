## Author: Stephen E. Porter
## Title: Random Forest (Task 2)
## Course: WGU D209: Data Mining I
## Instructor: Dr. Festus Elleh
options(warn=-1)

# Libraries
library(tidyverse)
library(caret)
library(randomForest)

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

# Convert character columns to factors so they can be used in randomForest

dfDropped[sapply(dfDropped, is.character)] <- lapply(dfDropped[sapply(dfDropped, is.character)], as.factor)
str(dfDropped)

# Split dfDropped into training and testing subsets
set.seed(22)
trainId = createDataPartition(dfDropped$Churn, times = 1, p = 0.7, list = FALSE)

dfTrain = dfDropped[trainId,]
dfTest = dfDropped[-trainId,]

# Summary Statistics
summary(dfTrain)
summary(dfTest)

# Export prepared data sets
write.csv(dfTrain, "C:\\WGU\\D209 Data Mining I\\PA Task 2\\D209_dfTrain.csv", row.names = FALSE)
write.csv(dfTest, "C:\\WGU\\D209 Data Mining I\\PA Task 2\\D209_dfTest.csv", row.names = FALSE)

# Random Forest
rf <- randomForest(Churn~., data = dfTrain, proximity=TRUE)
rf

pred <- predict(rf, dfTest)
confusionMatrix(pred, dfTest$Churn)

str(pred)
churnPred<- ifelse(as.character(pred) == "Yes", 1, 0)
churnActual <- ifelse(as.character(dfTest$Churn) == "Yes", 1, 0)

mse <- mean((churnActual - churnPred)^2)

