source('~/projects/data-mining/hw2/incomeKNN.r')

setwd("/home/soren/projects/data-mining/hw2")
train_data = read.csv("income_tr.csv")
test_data = read.csv("income_te.csv")[1:100,]
test_labels = vector(length = nrow(test_data), mode = 'numeric')
test_labels[test_data$class == ' <=50K'] = 1
test_labels[test_data$class == ' >50K'] = 2
train_labels = vector(length = nrow(train_data), mode = 'numeric')
train_labels[train_data$class == ' <=50K'] = 1
train_labels[train_data$class == ' >50K'] = 2
test_data = incomeScale(train_data[,1:ncol(train_data)-1], test_data[,1:ncol(test_data)-1])
train_data = incomeScale(train_data[,1:ncol(train_data)-1], train_data[,1:ncol(train_data)-1])
pred = incomeKnn(5, train_data, train_labels, test_data, test_labels)
pred2 = incomeKnn(2, train_data, train_labels, test_data, test_labels)
pred10 = incomeKnn(10, train_data, train_labels, test_data, test_labels)
pred20 = incomeKnn(20, train_data, train_labels, test_data, test_labels)
roc = computeROC(pred)
roc2 = computeROC(pred2)
roc10 = computeROC(pred10)
roc20 = computeROC(pred20)
jpeg('rocs.jpg', width=500, height=500)
plot(roc, type = "l", col = "red")
par(new=TRUE)
plot(roc2, type = "l", col = "green")
par(new=TRUE)
plot(roc10, type = "l", col = "blue")
par(new=TRUE)
plot(roc20, type = "l", col = "yellow")
legend("topleft", classes, cex = 1.3, fill = colors)
dev.off()

computePerformance(pred10)