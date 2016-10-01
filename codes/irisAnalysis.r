source('irisKNN.r')

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

#Change of the value of k here
prediction = irisKnn(5, train_data, train_labels, test_data, test_labels)
print(prediction)

