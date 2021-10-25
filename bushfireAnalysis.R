#This purpose of this script is to perform modelling on data collected
#Testing and comparison will be conducted to select the best performing model

#install libraries and packages

#this package contains tools for tree moodel analysis
#install.packages("tree")
library(tree)

#this package contains functions for bagging analysis
#install.packages("bagging")
library(adabag)

#this package contains tools for random tree modelling
#install.packages("randomForest")
library(randomForest)


#read bushfire data
bushfire.omit = read.csv("bushfire.csv")
#convert columns to the correct data type
bushfire.omit = bushfire.omit[,!(names(bushfire.omit) %in% c("Date"))]
bushfire.omit$Month = as.factor(bushfire.omit$Month)
for (i in 1:ncol(bushfire.omit)){
  if (class(bushfire.omit[,i]) == "character"){
    bushfire.omit[,i] = as.factor(bushfire.omit[,i])
  }
  if (class(bushfire.omit[,i]) == "integer"){
    bushfire.omit[,i] = as.numeric(bushfire.omit[,i])
  }
}

#set seed to ensure results are consistent
set.seed(123456) 

#seperate data into 70% training data and 30% testing data
train.row = sample(1:nrow(bushfire.omit), 0.7*nrow(bushfire.omit))
bushfire.train = bushfire.omit[train.row,]
bushfire.test = bushfire.omit[-train.row,]

#Tree model
#fitting the data into a tree model
bushfire.fit.tree = tree(Occurance ~.,data = bushfire.train)

#predict using the model against the testing data
bushfire.tree.pred = predict(bushfire.fit.tree, bushfire.test ,type = "class")

#create a confusion matrix for the prediction
tree.cm = table(Predicted_Class = bushfire.tree.pred, Observed_Class = bushfire.test$Occurance)
tree.cm

#calculate the accuracy of the model
tree.accuracy = (tree.cm[1]+tree.cm[4])/nrow(bushfire.test)


#calculate the sensitivity, specificity, false negative rate
#and false positive rate of the prediction
tree.sensitivity = tree.cm[4]/(tree.cm[3] + tree.cm[4])
tree.specificity = tree.cm[1]/(tree.cm[1] + tree.cm[2])
tree.fnr = 1 - tree.sensitivity
tree.fpr = 1 - tree.specificity



#random forest model
#fitting the data into a random forest model
#limitation: time taken to train model is too long
bushfire.fit.rf = randomForest(Occurance ~ .  ,data = bushfire.train)

#predict using the model against the testing data
bushfire.rf.pred = predict(bushfire.fit.rf, bushfire.test)
#create a confusion matrix for the prediction
rf.cm = table(Predicted_Class = bushfire.rf.pred, Observed_Class = bushfire.test$Occurance)
rf.cm

#calculate the accuracy of the model
rf.accuracy = (rf.cm[1]+rf.cm[4])/nrow(bushfire.test)


#calculate the sensitivity, specificity, false negative rate
#and false positive rate of the prediction
rf.sensitivity = rf.cm[4]/(rf.cm[3] + rf.cm[4])
rf.specificity = rf.cm[1]/(rf.cm[1] + rf.cm[2])
rf.fnr = 1 - rf.sensitivity
rf.fpr = 1 - rf.specificity


#bagging model
#fitting the data into a bagging model
#limitation: time taken too long
bushfire.fit.bag = bagging(Occurance ~ .  ,data = bushfire.train)

#predict using the model against the testing data
bushfire.bag.pred = predict(bushfire.fit.bag, bushfire.test)

#display the confusion matrix for the prediction
bushfire.bag.pred$confusion

#calculate the accuracy of the model
bag.accuracy = 1-bushfire.bag.pred$error


#calculate the sensitivity, specificity, false negative rate
#and false positive rate of the prediction
bag.sensitivity = bushfire.bag.pred$confusion[4]/(bushfire.bag.pred$confusion[3] + bushfire.bag.pred$confusion[4])
bag.specificity = bushfire.bag.pred$confusion[1]/(bushfire.bag.pred$confusion[1] + bushfire.bag.pred$confusion[2])
bag.fnr = 1 - bag.sensitivity
bag.fpr = 1 - bag.specificity


#check important variables from models
summary(bushfire.fit.tree)
bushfire.fit.bag$importance
bushfire.fit.rf$importance

#set seed for consistent performance
set.seed(123456) 
#the random forest model was chosen for this project because it has the highest accuracy
#and lowest false negative rate among other models
#retrain random forest model to improve performance and reduce time taken to train
bushfire.fit.rf.ver2 = randomForest(Occurance ~ StateTerritory + MaxTemp + Month + WindDir9am + Temp3pm + 
                                      Pressure9am + Humidity3pm ,data = bushfire.train, ntree = 90, mtry = 3)

#predict using the model against the testing data
bushfire.rf.pred.ver2 = predict(bushfire.fit.rf.ver2, bushfire.test)
#create a confusion matrix for the prediction
rf.cm.ver2 = table(Predicted_Class = bushfire.rf.pred.ver2, Observed_Class = bushfire.test$Occurance)
rf.cm.ver2

#calculate the accuracy of the model
rf.ver2.accuracy = (rf.cm.ver2[1]+rf.cm.ver2[4])/nrow(bushfire.test)


#calculate the sensitivity, specificity, false negative rate
#and false positive rate of the prediction
rf.ver2.sensitivity = rf.cm.ver2[4]/(rf.cm.ver2[3] + rf.cm.ver2[4])
rf.ver2.specificity = rf.cm.ver2[1]/(rf.cm.ver2[1] + rf.cm.ver2[2])
rf.ver2.fnr = 1 - rf.ver2.sensitivity
rf.ver2.fpr = 1 - rf.ver2.specificity

#collect value for comparison
values = c(tree.accuracy,tree.sensitivity,tree.specificity,tree.fnr,tree.fpr,
           bag.accuracy,bag.sensitivity,bag.specificity,bag.fnr,bag.fpr,
           rf.accuracy,rf.sensitivity,rf.specificity,rf.fnr,rf.fpr,
           rf.ver2.accuracy,rf.ver2.sensitivity,rf.ver2.specificity,rf.ver2.fnr,rf.ver2.fpr)

#create a table to compare performance of different models
comparisonTable = matrix(values ,ncol=5,byrow=TRUE)
colnames(comparisonTable) <- c("Accuracy","Sensitivity","Specificity", "FNR", "FPR")
rownames(comparisonTable) <- c("Decision Tree","Bagging","Random Forest", "Improved Random Forest")
comparisonTable <- as.table(comparisonTable)
comparisonTable





