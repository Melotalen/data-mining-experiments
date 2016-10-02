debugSource('~/projects/data-mining/hw2/incomeKNN.r')
setwd("/home/soren/projects/data-mining/hw2")
library(class)
train_data = read.csv("income_tr.csv")
#train_data = train_data[1:100,]
test_data = read.csv("income_te.csv")
#test_data = test_data[1:100,]
test_labels = vector(length = nrow(test_data), mode = 'numeric')
test_labels[test_data$class == ' <=50K'] = 1
test_labels[test_data$class == ' >50K'] = 2
train_labels = vector(length = nrow(train_data), mode = 'numeric')
train_labels[train_data$class == ' <=50K'] = 1
train_labels[train_data$class == ' >50K'] = 2
test_data = incomeScale(train_data[,1:ncol(train_data)-1], test_data[,1:ncol(test_data)-1])
train_data = incomeScale(train_data[,1:ncol(train_data)-1], train_data[,1:ncol(train_data)-1])
original_data = train_data
train_data = recodeIncomeData(original_data, train_data)
test_data = recodeIncomeData(original_data, test_data)


roc = computeROC(pred)
plot(roc, type = 'l')

#Plotting AUC
areas = vector(mode = 'numeric', length = 100)
for(i in 1:100){
  predk = knn(train_data, test_data, factor(train_labels), k = i, prob = TRUE)
  predk = normalizePredictions(predk)
  rock = computeROC(predk)
  areas[i] = computeAUC(rock)
}
jpeg('auc_curve.jpg', width=500, height=500)
plot(areas, type = "l", main = "Area Under the Curve for various values of k", xlab = "Number of Nearest Neighbors (k)", ylab = "Area Under the Curve")
dev.off()

#Geenrating ROC curves
pred5 = normalizePredictions(knn(train_data, test_data, factor(train_labels), k = 10, prob = TRUE))
pred2 = normalizePredictions(knn(train_data, test_data, factor(train_labels), k = 5, prob = TRUE))
pred10 = normalizePredictions(knn(train_data, test_data, factor(train_labels), k = 20, prob = TRUE))
pred29 = normalizePredictions(knn(train_data, test_data, factor(train_labels), k = 30, prob = TRUE))
roc5 = computeROC(pred5)
roc2 = computeROC(pred2)
roc10 = computeROC(pred10)
roc29 = computeROC(pred29)
jpeg('rocs.jpg', width=500, height=500)
classes = c("5", "10", "20","30")
plot(roc2, type = "l", col = "green", main = "ROC curves for Income Dataset classifier with k = 5, 10, 20 and 30")
par(new=TRUE)
plot(roc5, type = "l", col = "red")
par(new=TRUE)
plot(roc10, type = "l", col = "blue")
par(new=TRUE)
plot(roc29, type = "l", col = "cyan")
legend("bottomright", classes, cex = 1.3, fill = c("green", "red", "blue", "cyan"))
dev.off()

#Optimium Cost computation for k=33
labels = pred29[,1]
confidences = sort(pred29[,2])
thresh = vector(mode = 'numeric', length = length(confidences))
cost = vector(mode = 'numeric', length = length(confidences))
cost_matrix = matrix(c(1,0,0,1), nrow = 2, ncol = 2 )
for(i in 1:length(confidences)){
  thresh[i] = confidences[1] + diff(range(confidences))*i/length(confidences)
  mat = computeConfusionMatrix(labels, confidences, thresh[i])
  cost[i] = sum(cost_matrix*mat)/sum(mat)
}
jpeg('income_cost.jpg', width=500, height=500)
plot(thresh, cost, type = "l", col = 'blue', main = "Accuracy Vs Threshold k = 30", xlab = "Threshold", ylab = "Accuracy")
dev.off()

labels = pred29[,1]
confidences = sort(pred29[,2])
thresh = vector(mode = 'numeric', length = length(confidences))
precision = vector(mode = 'numeric', length = length(confidences))
recall = vector(mode = 'numeric', length = length(confidences))
fmeasure = vector(mode = 'numeric', length = length(confidences))
cost_matrix = matrix(c(1,0,0,1), nrow = 2, ncol = 2 )
for(i in 1:length(confidences)){
  thresh[i] = confidences[1] + diff(range(confidences))*i/length(confidences)
  mat = computeConfusionMatrix(labels, confidences, thresh[i])
  precision[i] = mat[1,1]/(mat[1,1] + mat[2,1])
  recall[i] = mat[1,1]/sum(mat[1,])
  fmeasure[i] = 2*precision[i]*recall[i]/(precision[i] +  recall[i])
}
jpeg('prf.jpg', width=500, height=500)
plot(thresh, recall, type = "l", col = 'green', main = "Precision, Recall and F-Measure Vs Threshold k = 29", xlab = "Threshold", ylab = "Value")
lines(thresh, precision, type = "l", col = 'blue', main = "Precision, Recall and F-Measure Vs Threshold k = 29", xlab = "Threshold", ylab = "Value")
lines(thresh, fmeasure, type = "l", col = 'red', main = "Precision, Recall and F-Measure Vs Threshold k = 29", xlab = "Threshold", ylab = "Value")
legend("topright", c("Recall", "Precision", "F-Measure"), cex = 1.3, fill = c("green", "blue", "red"))
dev.off()

normalizePredictions = function(prediction){
  pred = data.frame(matrix(nrow = length(test_labels), ncol = 2))  
  names(pred) = c("Actual_Label", "Confidence")
  pred[,1] = test_labels
  probs = attr(prediction, "prob")
  probs[prediction == 1] = 1 - probs[prediction == 1]
  pred[,2] = probs
  return(pred)
}