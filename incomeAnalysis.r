source('~/projects/data-mining/hw2/incomeKNN.r')

setwd("/home/soren/projects/data-mining/hw2")
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
model = generateIncomeModel(train_data, train_labels, test_data)

#AUC Curve
areas = vector(mode = 'numeric', length = 100)
for(i in 1:100){
  predk = incomeKnn(i, model, test_labels)
  rock = computeROC(predk)
  areas[i] = computeAUC(rock)
}
jpeg('auc_curve.jpg', width=500, height=500)
plot(areas, type = "l", main = "Area Under the Curve for various values of k", xlab = "Number of Nearest Neighbors (k)", ylab = "Area Under the Curve")
dev.off()

#Geenrating ROC curves
pred5 = incomeKnn(5, model, test_labels)
pred2 = incomeKnn(2, model, test_labels)
pred10 = incomeKnn(10, model, test_labels)
pred36 = incomeKnn(36, model, test_labels)
roc5 = computeROC(pred5)
roc2 = computeROC(pred2)
roc10 = computeROC(pred10)
roc36 = computeROC(pred36)
jpeg('rocs.jpg', width=500, height=500)
classes = c("2", "5", "10","36")
plot(roc2, type = "l", col = "green", main = "ROC curves for Income Dataset classifier with k = 2, 5, 10 and 36")
par(new=TRUE)
plot(roc5, type = "l", col = "red")
par(new=TRUE)
plot(roc10, type = "l", col = "blue")
par(new=TRUE)
plot(roc36, type = "l", col = "cyan")
legend("bottomright", classes, cex = 1.3, fill = c("green", "red", "blue", "cyan"))
dev.off()

#Optimium Cost computation for k=33

labels = pred36[,1]
confidences = sort(pred36[,2])
thresh = vector(mode = 'numeric', length = length(confidences))
cost = vector(mode = 'numeric', length = length(confidences))
cost_matrix = matrix(c(1,0,0,1), nrow = 2, ncol = 2 )
for(i in 1:length(confidences)){
  thresh[i] = confidences[1] + diff(range(confidences))*i/length(confidences)
  mat = computeConfusionMatrix(labels, confidences, thresh[i])
  cost[i] = sum(cost_matrix*mat)/sum(mat)
}
jpeg('income_cost.jpg', width=500, height=500)
plot(thresh, cost, type = "l", col = 'blue', main = "Cost Vs Threshold Plot for classifier with k = 60", xlab = "Threshold", ylab = "Cost")
dev.off()

