norm_vec <- function(x) sqrt(sum(x^2))

recodeIncomeData = function(data, data2){
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country")
  data.nominal = data[,attr.nominal]
  data2.nominal = data2[,attr.nominal]
  attr.other = c("age", "capital_gain", "capital_loss", "hour_per_week", "education_cat")
  data2.other = data2[,attr.other]
  headings = c()
  for(i in 1:length(attr.nominal)){
    lvls = levels(data.nominal[,attr.nominal[i]])
    for(j in 1:length(lvls)){
      tag = paste(attr.nominal[i], lvls[j])
      headings = c(headings, tag)
    }
  }
  recoded.nominal = data.frame(matrix(nrow = nrow(data2.nominal), ncol = length(headings)))
  for(i in 1:nrow(recoded.nominal))
    for(j in 1:ncol(recoded.nominal))
      recoded.nominal[i,j] = 0
  names(recoded.nominal) = headings
  for(i in 1:nrow(recoded.nominal)){
    record = as.matrix(data2.nominal[i,])
    for(j in 1:ncol(record)){
      tag = paste(attr.nominal[j], record[j])
      recoded.nominal[i,tag] = 1
    }
  }
  return(data.frame(data2.other,recoded.nominal))
  
}

incomeScale = function(source_data, data) {
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country", "education_cat")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  #attr.ordinal = c()
  source_data.nominal = source_data[,attr.nominal]
  source_data.numeric = source_data[,attr.numeric]
  #source_data.ordinal = data.frame(source_data[,attr.ordinal])
  data.nominal = data[, attr.nominal]
  data.numeric = data[,attr.numeric]
  #data.ordinal = data.frame(data[,attr.ordinal])
  
  #Preprocessing Nominal Data
  data.nominal[data.nominal$workclass != " Private"] = " Non-Private"
  
  data.nominal[data.nominal$marital_status == " Married-AF-spouse" || data.nominal$marital_status == " Married-civ-spouse"] = " Married"
  data.nominal[data.nominal$marital_status != " Married-AF-spouse" && data.nominal$marital_status != " Married-civ-spouse"] = " Unmarried"
  
  data.nominal[data.nominal$occupation == " Exec-managerial" ||
                 data.nominal$occupation == " Prof-specialty" ||
                 data.nominal$occupation == " Farming-fishing"] = " High-Income-Occupation"
  data.nominal[data.nominal$occupation != " Exec-managerial" &&
                 data.nominal$occupation != " Prof-specialty" &&
                 data.nominal$occupation != " Farming-fishing"] = " Low-Income-Occupation"
  
  data.nominal[data.nominal$native_country != " United-States"] = " Non-US"
  
  data.nominal[data.nominal$education_cat <= 8 || 
                 data.nominal$education_cat >= 13] = "Favourable-Education"
  data.nominal[data.nominal$education_cat > 8 && 
                 data.nominal$education_cat < 13] = "Unfavourable-Education"
  data.nominal[data.nominal$race != " White"] = " Non-White"
  
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
  # means <- NULL
  # for(i in 1:ncol(source_data.ordinal)) {
  #   means[i] <- mean(source_data.ordinal[,i])  
  # }
  # sdev <- NULL
  # for(i in 1:ncol(source_data.ordinal)){
  #   sdev[i] <- sd(source_data.ordinal[,i])
  # }
  # output.ordinal = data.frame(matrix(ncol = ncol(data.ordinal), nrow = nrow(data.ordinal)))
  # for(j in 1:nrow(data.ordinal)){
  #   output.ordinal[j,] <- (data.ordinal[j,] - means)/sdev
  # }
  # names(output.ordinal) = attr.ordinal
  output.nominal = data.nominal
  output = data.frame(output.nominal, output.numeric, output.ordinal)
  return(output)
}

getIncomePrediction = function(distances, labels, k){
  classWeight = c(0,0)
  for(i in 1:k){
    classWeight[labels[i]] = classWeight[labels[i]] + 1/distances[i]
  }
  return(classWeight[2]/sum(classWeight))
}

generateIncomeModel = function(train_data, train_labels, test_data){
  #Creating Datasets Income_original.csv
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country", "education_cat")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  #attr.ordinal = c()
  for(i in 1:ncol(test_data))
    levels(test_data[,i]) = levels(train_data[,i])
  train_data.nominal = train_data[,attr.nominal]
  train_data.numeric = train_data[,attr.numeric]
  #train_data.ordinal = data.frame(train_data[,attr.ordinal])
  test_data.nominal = test_data[, attr.nominal]
  test_data.numeric = test_data[,attr.numeric]
  #test_data.ordinal = data.frame(test_data[,attr.ordinal])
  #Creating output dataframe
  output = array(dim = c(nrow(test_data), 2, nrow(train_data))) #test x 2 x train
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
  n_ordinal = 0 #ncol(test_data.ordinal)
  n_total = n_nominal + n_numeric + n_ordinal
  for(i in 1:nrow(test_data)){
    print(paste("test element:",i))
    distances.nominal = vector(mode = "numeric", length = nrow(train_data))
    distances.numeric = vector(mode = "numeric", length = nrow(train_data))
    distances.ordinal = vector(mode = "numeric", length = nrow(train_data))
    for(j in 1:nrow(train_data)){
      distances.nominal[j] = nominalDissimilarity(train_data.nominal[j,], test_data.nominal[i,])
      distances.numeric[j] = sqrt(sum((train_data.numeric[j,] - test_data.numeric[i,])^2))
     distances.ordinal[j] = 0 #sqrt(sum((train_data.ordinal[j,] - test_data.ordinal[i,])^2))
    }
    distances.total = (n_nominal*distances.nominal + n_numeric*distances.numeric + n_ordinal*distances.ordinal)/(n_total)
    distances.total.sorted = sort(distances.total, index.return = TRUE)
    distances.total.sorted$ix = train_labels[distances.total.sorted$ix]
    output[i,1,] = distances.total.sorted$x #Distances soreted in increaing order
    output[i,2,] = distances.total.sorted$ix #Labels sorted by distances
  }
  return(output)
}

generateIncomeModelCosine = function(train_data, train_labels, test_data){
  #Creating Datasets Income_original.csv
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country", "education_cat")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  #attr.ordinal = c("education_cat")
  for(i in 1:ncol(test_data))
    levels(test_data[,i]) = levels(train_data[,i])
  train_data.nominal = train_data[,attr.nominal]
  train_data.numeric = train_data[,attr.numeric]
  #train_data.ordinal = data.frame(train_data[,attr.ordinal])
  test_data.nominal = test_data[, attr.nominal]
  test_data.numeric = test_data[,attr.numeric]
  #test_data.ordinal = data.frame(test_data[,attr.ordinal])
  #Creating output dataframe
  output = array(dim = c(nrow(test_data), 2, nrow(train_data))) #test x 2 x train
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
  n_ordinal = 0 #ncol(test_data.ordinal)
  n_total = n_nominal + n_numeric + n_ordinal
  for(i in 1:nrow(test_data)){
    print(paste("test element:",i))
    distances.nominal = vector(mode = "numeric", length = nrow(train_data))
    distances.numeric = vector(mode = "numeric", length = nrow(train_data))
    distances.ordinal = vector(mode = "numeric", length = nrow(train_data))
    for(j in 1:nrow(train_data)){
      distances.nominal[j] = nominalDissimilarity(train_data.nominal[j,], test_data.nominal[i,])
      cosinediss = sum(train_data.numeric[j,]*test_data.numeric[i,])/norm_vec(train_data.numeric[j,])/norm_vec(test_data.numeric[i,])
      if(cosinediss > 1) cosinediss = 1
      if(cosinediss < -1) cosinediss = -1
      distances.numeric[j] = acos(cosinediss)
      distances.ordinal[j] = 0#sqrt(sum((train_data.ordinal[j,] - test_data.ordinal[i,])^2))
    }
    distances.total = (n_nominal*distances.nominal + n_numeric*distances.numeric + n_ordinal*distances.ordinal)/(n_total)
    distances.total.sorted = sort(distances.total, index.return = TRUE)
    distances.total.sorted$ix = train_labels[distances.total.sorted$ix]
    output[i,1,] = distances.total.sorted$x #Distances soreted in increaing order
    output[i,2,] = distances.total.sorted$ix #Labels sorted by distances
  }
  return(output)
}
incomeKnn = function(k, model, test_labels){
  output = data.frame(matrix(nrow = length(test_labels), ncol = 2))  
  names(output) = c("Actual_Label", "Confidence")
  for(i in 1:length(test_labels)){
    output[i,1] = test_labels[i]
    output[i,2] = getIncomePrediction(model[i,1,1:k],model[i,2,1:k],k)
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

generateTable = function(prediction){
  output = data.frame(matrix(nrow = nrow(prediction), ncol = 4))  
  names(output) = c("Transaction_ID", "Actual_Label", "Predicted_Label", "Confidence")
  for(i in 1:nrow(model)){
    output[i,1] = i
    output[i,2] = prediction$Actual_Label[i]
    if(prediction$Confidence[i] > 0.5)
      output[i,3] = 2
    else
      output[i,3] = 1
    output[i,4] = prediction$Confidence[i]
  }
  return(output)
}
