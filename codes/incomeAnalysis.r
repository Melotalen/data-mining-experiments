source('~/projects/data-mining/hw2/incomeKNN.r')

setwd("/home/soren/projects/data-mining/hw2")
train_data = read.csv("income_tr.csv")
test_data = read.csv("income_te.csv")
test_labels = vector(length = nrow(test_data), mode = 'numeric')
test_labels[test_data$class == ' <=50K'] = 1
test_labels[test_data$class == ' >50K'] = 2
train_labels = vector(length = nrow(train_data), mode = 'numeric')
train_labels[train_data$class == ' <=50K'] = 1
train_labels[train_data$class == ' >50K'] = 2
test_data = incomeScale(train_data[,1:ncol(train_data)-1], test_data[,1:ncol(test_data)-1])
train_data = incomeScale(train_data[,1:ncol(train_data)-1], train_data[,1:ncol(train_data)-1])
model = generateIncomeModel(train_data, train_labels, test_data)
pred = incomeKnn(5, model, test_labels)
print(generateTable(pred))

