
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

getIncomePrediction = function(indices, labels){
  classCount = c(0,0)
  for(i in 1:length(indices)){
    classCount[labels[indices[i]]] = classCount[labels[indices[i]]] + 1
  }
  maxindex = 1
  if(classCount[2] > classCount[1])
    maxindex = 2
  
  pred = c(maxindex, classCount[2]/sum(classCount))
  names(pred) = c("class", "probability")
  return(pred)
}

incomeKnn = function(k, train_data, train_labels, test_data, test_labels){
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
    output = data.frame(matrix(nrow = nrow(test_data), ncol = 4))
    names(output) = c("Transaction_ID", "Actual_Label", "Predicted_Label", "Confidence")
    
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
    for(i in 1:nrow(test_data)){
      print(paste("test element:",i))
      distances.nominal = vector(mode = "numeric", length = nrow(train_data))
      distances.numeric = vector(mode = "numeric", length = nrow(train_data))
      distances.ordinal = vector(mode = "numeric", length = nrow(train_data))
      for(j in 1:nrow(train_data)){
        distances.nominal[j] = nominalDissimilarity(train_data.nominal[j,], test_data.nominal[i,])
        distances.numeric[j] = sqrt(sum((train_data.numeric[i,] - test_data.numeric[j,])^2))
        distances.ordinal[j] = sqrt(sum((train_data.ordinal[i,] - test_data.ordinal[j,])^2))
      }
      distances.total = (ncol(test_data.nominal)*distances.nominal + ncol(test_data.numeric)*distances.numeric + ncol(test_data.ordinal)*distances.ordinal)/(ncol(test_data.nominal) + ncol(test_data.numeric) + ncol(test_data.ordinal))
      distances.total.sorted = sort(distances.total, index.return = TRUE)
      prediction = getIncomePrediction(distances.total.sorted$ix[1:k], train_labels)
      output[i,1] = i
      output[i,2] = test_labels[i]
      output[i,3] = prediction["class"] 
      output[i,4] = prediction["probability"]
    }
    return(output)
  }
generateModel = function()

computeROC = function(predictions){
  negatives = nrow(predictions[predictions$Actual_Label == 1,])
  positives = nrow(predictions[predictions$Actual_Label == 2,])
  predictions = predictions[order(predictions$Confidence),]
  roc = data.frame()#matrix(nrow = nrow(predictions), ncol = 2))
  roc[1,1] = nrow(predictions[predictions$Actual_Label == 1,])
  roc[1,2] = nrow(predictions[predictions$Actual_Label == 2,])
  for(i in 1:nrow(predictions)){
    if(predictions$Actual_Label[i] == 1){
      roc[i+1,1] = roc[i,1] - 1
      roc[i+1,2] = roc[i,2]
      }
    else if(predictions$Actual_Label[i] == 2){
      roc[i+1,1] = roc[i,1] 
      roc[i+1,2] = roc[i,2] - 1
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