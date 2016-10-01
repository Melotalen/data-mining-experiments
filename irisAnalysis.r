source('~/projects/data-mining/hw2/irisKNN.r')

setwd("/home/soren/projects/data-mining/hw2")
train_data = read.csv("Iris.csv")
test_data = read.csv("Iris_Test.csv")
test_labels = vector(length = nrow(test_data), mode = 'numeric')
test_labels[test_data[,5] == 'Iris-setosa'] = 1
test_labels[test_data[,5] == 'Iris-versicolor'] = 2
test_labels[test_data[,5] == 'Iris-virginica'] = 3
train_labels = vector(length = nrow(train_data), mode = 'numeric')
train_labels[train_data[,5] == 'Iris-setosa'] = 1
train_labels[train_data[,5] == 'Iris-versicolor'] = 2
train_labels[train_data[,5] == 'Iris-virginica'] = 3
test_data = irisScale(train_data[,1:4], test_data[,1:4])
train_data = irisScale(train_data[,1:4], train_data[,1:4])


print("K=1")
prediction = irisKnn(1, train_data, train_labels, test_data, test_labels)
mat = computeConfusionMatrix(prediction)
print(mat)
print("##################################")
# print("K=3")
# prediction = irisKnn(3, train_data, train_labels, test_data, test_labels)
# computePerformance(prediction)
# print("##################################")
print("K=5")
prediction = irisKnn(5, train_data, train_labels, test_data, test_labels)
mat = computeConfusionMatrix(prediction)
print(mat)
print("##################################")
print("K=10")
prediction = irisKnn(10, train_data, train_labels, test_data, test_labels)
mat = computeConfusionMatrix(prediction)
print(mat)
print("##################################")
# print("K=20")
# prediction = irisKnn(20, train_data, train_labels, test_data, test_labels)
# computePerformance(prediction)
# print("##################################")

