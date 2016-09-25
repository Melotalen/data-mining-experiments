
incomeScale = function(source_data, data) {
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  attr.ordinal = c("education_cat")
  source_data.nominal = source_data[,attr.nominal]
  source_data.numeric = source_data[,attr.numeric]
  source_data.ordinal = data.frame(source_data[,attr.ordinal])
  data.nominal = data[, attr.nominal]
  data.numeric = data[,attr.numeric]
  data.ordinal = data.frame(data[,attr.ordinal])
  
  #Normalizing Numeric Data
  means <- NULL
  for(i in 1:ncol(source_data.numeric)) {
    means[i] <- mean(source_data.numeric[,i])  
  }
  sdev <- NULL
  for(i in 1:ncol(source_data.numeric)){
    sdev[i] <- sd(source_data.numeric[,i])
  }
  output.numeric = data.frame(matrix(ncol = ncol(data.numeric), nrow = nrow(data.numeric)))
  for(j in 1:nrow(data.numeric)){
    output.numeric[j,] <- (data.numeric[j,] - means)/sdev
  }
  names(output.numeric) = attr.numeric
  #Normalizing Ordinal Data
  means <- NULL
  for(i in 1:ncol(source_data.ordinal)) {
    means[i] <- mean(source_data.ordinal[,i])  
  }
  sdev <- NULL
  for(i in 1:ncol(source_data.ordinal)){
    sdev[i] <- sd(source_data.ordinal[,i])
  }
  output.ordinal = data.frame(matrix(ncol = ncol(data.ordinal), nrow = nrow(data.ordinal)))
  for(j in 1:nrow(data.ordinal)){
    output.ordinal[j,] <- (data.ordinal[j,] - means)/sdev
  }
  names(output.ordinal) = attr.ordinal
  output.nominal = data.nominal
  output = data.frame(output.nominal, output.numeric, output.ordinal)
  return(output)
}

getIncomePrediction = function(labels){
  classCount = c(0,0)
  for(i in 1:length(labels)){
    classCount[labels[i]] = classCount[labels[i]] + 1
  }
  return(classCount[2]/sum(classCount))
}

generateIncomeModel = function(train_data, train_labels, test_data){
  #Creating Datasets Income_original.csv
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  attr.ordinal = c("education_cat")
  for(i in 1:ncol(test_data))
    levels(test_data[,i]) = levels(train_data[,i])
  train_data.nominal = train_data[,attr.nominal]
  train_data.numeric = train_data[,attr.numeric]
  train_data.ordinal = data.frame(train_data[,attr.ordinal])
  test_data.nominal = test_data[, attr.nominal]
  test_data.numeric = test_data[,attr.numeric]
  test_data.ordinal = data.frame(test_data[,attr.ordinal])
  #Creating output dataframe
  output = matrix(nrow = nrow(test_data), ncol = nrow(train_data))
  #Function for computing nominal dissimilarity
  nominalDissimilarity = function(val1, val2){
    distance = 0
    for(i in 1:length(val1)){
      if(val1[i] != val2[i])
        distance = distance + 1
    }
    return(distance)
  }
  
  #Actual COmputation begins here
  n_nominal = ncol(test_data.nominal)
  n_numeric = ncol(test_data.numeric)
  n_ordinal = ncol(test_data.ordinal)
  n_total = n_nominal + n_numeric + n_ordinal
  for(i in 1:nrow(test_data)){
    print(paste("test element:",i))
    distances.nominal = vector(mode = "numeric", length = nrow(train_data))
    distances.numeric = vector(mode = "numeric", length = nrow(train_data))
    distances.ordinal = vector(mode = "numeric", length = nrow(train_data))
    for(j in 1:nrow(train_data)){
      distances.nominal[j] = nominalDissimilarity(train_data.nominal[j,], test_data.nominal[i,])
      distances.numeric[j] = sqrt(sum((train_data.numeric[j,] - test_data.numeric[i,])^2))
      distances.ordinal[j] = sqrt(sum((train_data.ordinal[j,] - test_data.ordinal[i,])^2))
    }
    distances.total = (n_nominal*distances.nominal + n_numeric*distances.numeric + n_ordinal*distances.ordinal)/(n_total)
    distances.total.sorted = sort(distances.total, index.return = TRUE)
    output[i,] = train_labels[distances.total.sorted$ix]
  }
  return(output)
}

incomeKnn = function(k, model, test_labels){
  output = data.frame(matrix(nrow = nrow(model), ncol = 2))  
  names(output) = c("Actual_Label", "Confidence")
  for(i in 1:nrow(model)){
    output[i,1] = test_labels[i]
    output[i,2] = getIncomePrediction(model[i,1:k])
  }
  return(output)
}

computeROC = function(predictions){
  negatives = nrow(predictions[predictions$Actual_Label == 1,])
  positives = nrow(predictions[predictions$Actual_Label == 2,])
  predictions = predictions[order(predictions$Confidence, decreasing = TRUE),]
  roc = data.frame()#matrix(nrow = nrow(predictions), ncol = 2))
  roc[1,1] = 0
  roc[1,2] = 0
  for(i in 1:nrow(predictions)){
    if(predictions$Actual_Label[i] == 1){
      roc[i+1,1] = roc[i,1] + 1
      roc[i+1,2] = roc[i,2]
      }
    else if(predictions$Actual_Label[i] == 2){
      roc[i+1,1] = roc[i,1] 
      roc[i+1,2] = roc[i,2] + 1
    }
  }
  roc[,1] = roc[,1]/negatives
  roc[,2] = roc[,2]/positives
  return(roc)
}

computePerformance = function(predictions){
  correct = 0
  incorrect = 0
  classification_confidence = 0
  for(i in 1:nrow(predictions)){
    if(predictions$Actual_Label[i] == predictions$Predicted_Label[i]){
      correct = correct + 1
      classification_confidence = classification_confidence + predictions$Confidence[i]
    }
    else
      incorrect = incorrect + 1
  }
  print(paste("Classification Rate:", correct/nrow(predictions),"   Classification Confidence:", classification_confidence/correct))
  print(paste("Misclassification Rate:", incorrect/nrow(predictions)))
}

computeAUC = function(curve) {
   area = 0
   for(i in 2:nrow(curve)){
     area = area + curve[i,2]*(curve[i,1] - curve[i-1,1])
   }
   return(area)
}
#       Predicted
#Actual TP  FN
#       FP  TN
computeConfusionMatrix = function(labels, confidences, threshold){
  confusion_matrix = matrix(data = c(0,0,0,0), nrow = 2, ncol = 2)
  for(i in 1:length(confidences)){
    if(confidences[i] <= threshold)
      pred_label = 1
    else
      pred_label = 2
    if(labels[i] == pred_label && labels[i] == 2) #TP
      confusion_matrix[1,1] = confusion_matrix[1,1] + 1
    else if(labels[i] != pred_label && labels[i] == 2)
      confusion_matrix[1,2] = confusion_matrix[1,2] + 1
    else if(labels[i] != pred_label && labels[i] == 1)
      confusion_matrix[2,1] = confusion_matrix[2,1] + 1
    else if(labels[i] == pred_label && labels[i] == 1)
      confusion_matrix[2,2] = confusion_matrix[2,2] + 1
  }
  return(confusion_matrix)
}