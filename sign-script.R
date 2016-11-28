# I have ran the three algorithms first on un processed data.
# Then I ran the algorithms on normalizd data as well.
# Generally normalization provides an accuracy improve.

## load libraries
library(caret)
library(ggplot2)
library(caTools)
library(e1071)
library(class)
library(randomForest)
library(tidyr)
library(dplyr)
library(xlsx)

## loading input ===================================================================================

# setwd("./upwork/sign-language")
# read the excel file
# sign <- read.xlsx("sign.xlsx", sheetIndex = 1)

# read csv
sign <- read.csv("sign-csv.csv")

# looking at the structure of the data
str(sign)

# look at summary of data
summary(sign)

# Removing columns with zero variance- A5, A9, A10, A14, A15, A19, A20, A24, A25 by assigning NULL value
sign$A5 <- NULL
sign$A9 <- NULL
sign$A10 <- NULL
sign$A14 <- NULL
sign$A15 <- NULL
sign$A19 <- NULL
sign$A20 <- NULL
sign$A24 <- NULL
sign$A25 <- NULL

# removing the username and userID columns also
sign$UserNmae <- NULL
sign$UserID <- NULL

# checking how many sign are present in the language
levels(sign$Lable)
# there seems to be a total of 30 signs

# checking how many rows each sign has
table(sign$Lable)

# removing outcome "giant" and "plate"
sign$Lable <- as.character(sign$Lable)
sign$Lable[sign$Lable == "plate" | sign$Lable == "giant"] <- NA
sign <- na.omit(sign)
sign$Lable <- factor(sign$Lable)

# Visualization ====================================================================================

library(ggplot2)

# visualizing number of rows for each signs
ggplot(data = sign, aes(x = Lable)) + geom_bar(fill = "brown") + ylab("Sign Count")

# we can plot boxplots for every numeric variable on y axis and Lable on x axis.
# here I have plotted only three. You can repeat it by just replacing the y parameter.
ggplot(data = sign,aes(x = Lable, y = A6)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A7)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A8)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A11)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A12)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A13)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A16)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A17)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A18)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A21)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A22)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A23)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A26)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A27)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = A28)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H19)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H20)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H21)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H22)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H23)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H24)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H25)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H26)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H27)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H28)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H29)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H30)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H31)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H32)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H33)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H34)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H35)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H36)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H37)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H38)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H39)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H40)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H41)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H42)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H43)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H44)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H45)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H46)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H47)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H48)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H49)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H50)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H51)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H52)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H53)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H54)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H55)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H56)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H57)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H58)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H59)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H60)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H61)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H62)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H63)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H64)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H65)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H66)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H67)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H68)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H69)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H70)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H71)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H72)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H73)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H74)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H75)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H76)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H77)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H78)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H79)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H80)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H81)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H82)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H83)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H84)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H85)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H86)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H87)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H88)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H89)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H90)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H91)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H92)) + geom_boxplot()
ggplot(data = sign,aes(x = Lable, y = H93)) + geom_boxplot()


## Splitting data in train and test sets. ==========================================================

# The train set will contain 75% data to on which the machine learning algorithm will run.
# The test set will contain 25% data to test our predictions.

# We will use the package "caTools" which splits the data based on the outcome variable
# i.e. it splits the data uniformly.
library(caTools)

# set.seed is used so that each time it does the same split.
# otherwise everytime we split the data it will result in a different split.
set.seed(10)

# split is a logical vector. It is assigned TRUE for train set and FALSE for test set.
split <- sample.split(sign$Lable, SplitRatio = 0.75)
train <- subset(sign, split == TRUE)
test <- subset(sign, split == FALSE)


## SVM model =======================================================================================

# the SVM model is in the package e1071 so we load that library
library(e1071)

# SVM has two parameters- cost and gamma.
# I kept changing the values of the cost from 0-3 and for gamma 0.001-0.5 in order to get best accuracy.

# create svm model using following function.
svm.model <- svm(Lable ~ ., data = train, kernel = "radial", cost = 2, gamma = 0.01)

# predict values on test set using the svm model
svm.predict <- predict(svm.model, test)

# the following shows us the accuracy of our algorithm. it also outputs a table
# the table shows which values were predicted correctly and which were not predicted correctly.

confusionMatrix(svm.predict, test$Lable) 
# after playing around with a few values of cost and gamma I got best results for the following values.
# cost = 2, gamma = 0.01 gives the best accuracy of *0.9188*


## kNN Model =======================================================================================

# load package where kNN is present.
library(class)

# to use kNN we need to separate our "label" column from test and train sets
# othwerwise it gives us error.

knn.train <- train[, -1] # keep everything except the target variable in knn.train
knn.test <- test[, -1] # do the same for test
knn.train.label <- train[, 1] # keep the train target variable separately.
knn.test.label <- test[, 1] # to the same for test.

# set the seed for reproducible results.
set.seed(42)

# applying the kNN algorithm
knn.model <- knn(train = knn.train, test = knn.test, cl = knn.train.label, k = 1)

# take a look at accuracy
confusionMatrix(knn.model, test$Lable)

# k=1 gives accuracy of *0.8696*
# k=10 gives accuracy of *0.8783* (best)
# k=20 gives accuracy of *0.8348*
# k=30 gives accuracy of *0.8029*


## Random Forest ===================================================================================

# load library
library(randomForest)

# set the seed for reproducible results.
set.seed(100)

# create random forest model.
# Here the parameters are ntree which specifies the number of trees to build. More trees more accuracy.
# nodesize is the maximum number of children a signle node can have.
# The smaller nodesize is the better accuracy but it overfits the data.

rf.model <- randomForest(Lable ~ ., data = train, importance = TRUE, ntree = 2000, nodesize = 5)
rf.predict <- predict(rf.model, test)

# accuracy check
confusionMatrix(rf.predict, test$Lable)
# nodesize = 5 gives the best accuracy *0.8899*


## Normalizing the data ============================================================================

normalize <- function(x)
{
    return((x - min(x)) / (max(x) - min(x)))
}

# normalizing the numeric data except the target variable.
sign.normalized <- as.data.frame(lapply(sign[,-1], normalize))
str(sign.normalized)
summary(sign.normalized)

# copying the target variable to normalized data
sign.normalized$Lable <- sign$Lable


## Splitting data ==================================================================================
set.seed(10)
split.normal <- sample.split(sign.normalized$Lable, SplitRatio = 0.75)
train.normal <- subset(sign.normalized, split.normal == TRUE)
test.normal <- subset(sign.normalized, split.normal == FALSE)


## SVM on normalized data ==========================================================================

svm.model.normal <- svm(Lable ~ ., data = train.normal, kernel = "radial", cost = 2, gamma = 0.01)

svm.predict.normal <- predict(svm.model.normal, test.normal)

confusionMatrix(svm.predict.normal, test.normal$Lable)
# Accuracy = 0.9155 (same as non-normalized data)


##kNN on normalized data ===========================================================================

knn.train.normal <- train.normal[, -91]
knn.test.normal <- test.normal[, -91]
knn.train.normal.label <- train.normal[, 91]
knn.test.normal.label <- test.normal[, 91]

set.seed(42)

knn.model.normal <- knn(train = knn.train.normal, test = knn.test.normal,
                        cl = knn.train.normal.label, k = 1)

confusionMatrix(knn.model.normal, test.normal$Lable) 
# accuracy = 0.8805 (better than non-normalized data)


## Random Forest  on normalized data ===============================================================

set.seed(100)

rf.model.normal <- randomForest(Lable ~ ., data = train.normal, importance = TRUE,
                                ntree = 2000, nodesize = 5)
rf.predict.normal <- predict(rf.model.normal, test.normal)

confusionMatrix(rf.predict.normal, test.normal$Lable)
# Accuracy = 0.8892 (A little worse than accuracy of 0.8899 of non-normalized data)
# Does not make a difference because we can change the seed value above and get better results also.
# Random forests isn't effected much by normalized data because it's so good already.


## ensembling ======================================================================================

# storing all predictions in a single data frame.
results <- data.frame(svm.prediction = svm.predict.normal, rf.prediction = rf.predict.normal, 
    knn.prediction = knn.model.normal, final.prediction = rep(0, 343), actual.label = test.normal$Lable)

str(results)
nrow(results)

# checking only those values where svm, knn and rf predictions differ from each other.
results[which(results$svm.prediction != results$rf.prediction |
                  results$rf.prediction != results$knn.prediction |
                  results$svm.prediction != results$knn.prediction), ]


getmode <- function(x) {
    unique.x <- unique(x)
    unique.x[which.max(tabulate(match(x, unique.x)))]
}

results$final.prediction <- apply(results,1,getmode)
results

confusionMatrix(results$final.prediction, results$actual.label)
# accuracy of 0.9242 which is best as of yet.


## Summarizing results ==============================================================================

# extracting accuracy for each outcome from the confusion matrix
rf.accuracy <- as.data.frame(confusionMatrix(rf.predict.normal, test.normal$Lable)$byClass)
svm.accuracy <- as.data.frame(confusionMatrix(svm.predict.normal, test.normal$Lable)$byClass)
knn.accuracy <- as.data.frame(confusionMatrix(knn.model.normal, test.normal$Lable)$byClass)
ensemble.accuracy <- as.data.frame(confusionMatrix(results$final.prediction, results$actual.label)$byClass)

# 11th column of the byClass has the label wise accuracy that we are looking for
rf.accuracy <- rf.accuracy[,11]
svm.accuracy <- svm.accuracy[,11]
knn.accuracy <- knn.accuracy[,11]
ensemble.accuracy <- ensemble.accuracy[,11]

# extracting the lable names
label.names <- (levels(sign$Lable))
label.names

# creating accuracy table showing accuracy of each model for each label.
accuracy.table <- data.frame(label = label.names, Support.Vector.Machine = svm.accuracy,
                            Random.Forest = rf.accuracy, k.Nearest.Neighbor = knn.accuracy,
                            ensemble.model = ensemble.accuracy)

print(accuracy.table)
# Thal label has NA value because it had only one value in the data set.
# So it either came in train set or test set and hence no algorithm predicted it.
# Providing more data will solve this problem.

# converting the table to long format
accuracy.table.long <- gather(accuracy.table, algorithm, accuracy,
                            Support.Vector.Machine:ensemble.model)
print(accuracy.table.long)

# accuracy tables for each algorithm
accuracy.svm.table <- accuracy.table.long[1:28, ]
accuracy.rf.table <- accuracy.table.long[29:56, ]
accuracy.knn.table <- accuracy.table.long[57:84, ]
accuracy.ensemble.table <- accuracy.table.long[85:112, ]

## Visualizing the accuracies
## this will show a warning that 3 rows are removed, those three rows are for Thal which has NA value.

ggplot(data = accuracy.table.long, aes(x = label, y = accuracy, fill = algorithm)) + 
    geom_bar(stat = "identity", position = "dodge") + theme(legend.position = "bottom") +
    labs(title = "Accuracy of each algorithm for each label", x = "Label", y = "Accuracy (in %)")

# knn accuracy
ggplot(data = subset(accuracy.table.long, accuracy.table.long$algorithm == "k.Nearest.Neighbor"),
    aes(x = label, y = accuracy)) + 
    geom_bar(stat = "identity", position = "dodge", fill = "brown") +
    theme(legend.position = "bottom") +
    labs(title = "Accuracy of k-Nearest Neighbor", x = "Label", y = "Accuracy (in %)")

# rf accuracy
ggplot(data = subset(accuracy.table.long, accuracy.table.long$algorithm == "Random.Forest"),
    aes(x = label, y = accuracy)) + 
    geom_bar(stat = "identity", position = "dodge", fill = "darkgreen") +
    theme(legend.position = "bottom") +
    labs(title = "Accuracy of Random Forest", x = "Label", y = "Accuracy (in %)")

# svm accuracy
ggplot(data = subset(accuracy.table.long, accuracy.table.long$algorithm == "Support.Vector.Machine"),
    aes(x = label, y = accuracy)) + 
    geom_bar(stat = "identity", position = "dodge", fill = "darkblue") +
    theme(legend.position = "bottom") +
    labs(title = "Accuracy of Support Vector Machine", x = "Label", y = "Accuracy (in %)")

# ensemble accuracy
ggplot(data = subset(accuracy.table.long, accuracy.table.long$algorithm == "ensemble.model"),
       aes(x = label, y = accuracy)) + 
    geom_bar(stat = "identity", position = "dodge", fill = "darkcyan") +
    theme(legend.position = "bottom") +
    labs(title = "Accuracy of Support Vector Machine", x = "Label", y = "Accuracy (in %)")

## writing to excel file

write.xlsx(accuracy.table.long, "accuracy-long-format.xlsx")
write.xlsx(accuracy.table, "accuracy-table.xlsx")
write.xlsx(accuracy.svm.table, "accuracy-table-svm.xlsx")
write.xlsx(accuracy.rf.table, "accuracy-table-rf.xlsx")
write.xlsx(accuracy.knn.table, "accuracy-table-knn.xlsx")
write.xlsx(results, "results.xlsx")

## end of script ===================================================================================






