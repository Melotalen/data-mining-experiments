norm_vec <- function(x) sqrt(sum(x^2))

proximityIncomeEuclidean = function(k, df){
  #Reading Income_original.csv
  data.original = read.csv("~/projects/data-mining/hw1/income_tr.csv")
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  attr.ordinal = c("education_cat")
  data.nominal = data.original[,attr.nominal]
  data.numeric = data.original[,attr.numeric]
  data.ordinal = data.frame(data.original[,attr.ordinal])
  df.nominal = df[, attr.nominal]
  df.numeric = df[,attr.numeric]
  df.ordinal = data.frame(df[,attr.ordinal])
  #Creating output dataframe
  output <- data.frame(matrix(ncol = 2*k+1, nrow = nrow(df)))
  output.headings <- "Transaction ID"
  for( i in 1:k)
    output.headings <- append(output.headings, c(paste(i),paste(i,"-prox", sep="")), after = length(output.headings))
  colnames(output) <- output.headings
  
  #Normalizing numeric data
  data.numeric.normal = data.frame(scale(data.numeric))
  data.numeric.means = NULL
  for(i in 1:ncol(data.numeric)){
    data.numeric.means[i] <- mean(data.numeric[,i])  
  }
  data.numeric.sdev <- NULL
  for(i in 1:ncol(data.numeric)){
    data.numeric.sdev[i] <- sd(data.numeric[,i])
  }
  
  #Normalizing Ordinal Data
  data.ordinal.normal = data.frame(scale(data.ordinal))
  data.ordinal.means = NULL
  for(i in 1:ncol(data.ordinal)){
    data.ordinal.means[i] <- mean(data.ordinal[,i])  
  }
  data.ordinal.sdev <- NULL
  for(i in 1:ncol(data.ordinal)){
    data.ordinal.sdev[i] <- sd(data.ordinal[,i])
  }
  nominalDissimilarity = function(val1, val2){
    distance = 0
    for(i in 1:length(val1)){
      if(val1[i] != val2[i])
        distance = distance + 1
    }
    return(distance)
  }
  distances.nominal = vector(mode = "numeric", length = nrow(data.original))
  distances.numeric = vector(mode = "numeric", length = nrow(data.original))
  distances.ordinal = vector(mode = "numeric", length = nrow(data.original))
  for(i in 1:nrow(df)){
    record.nominal = data.frame(df.nominal[i,])
    record.numeric = data.frame((df.numeric[i,] - data.numeric.means)/data.numeric.sdev)
    record.ordinal = data.frame((df.ordinal[i,] - data.ordinal.means)/data.ordinal.sdev)
    for(j in 1:nrow(data.original)){
      distances.nominal[j] = nominalDissimilarity(record.nominal[i,],data.nominal[j,])
      distances.numeric[j] = sqrt(sum((record.numeric[i,] - data.numeric.normal[j,])^2))
      distances.ordinal[j] = sqrt(sum((record.ordinal[i,] - data.ordinal.normal[j,])^2))
    }
    distances.total = (ncol(data.nominal)*distances.nominal + ncol(data.numeric)*distances.numeric + ncol(data.ordinal)*distances.ordinal)/(ncol(data.nominal) + ncol(data.numeric) + ncol(data.ordinal))
    distances.total.sorted = sort(distances.total, index.return = TRUE)
    output[i,1] = i
    for(j in 1:k){
      output[i, 2*j] = distances.total.sorted$ix[j]
      output[i, 2*j+1] = distances.total.sorted$x[j]
    }
  }
  return(output)
}

proximityIncomeCosine = function(k, df){
  #Reading Income_original.csv
  data.original = read.csv("~/projects/data-mining/hw1/income_tr.csv")
  attr.nominal = c("workclass", "marital_status", "occupation", "relationship", "race", "gender", "native_country")
  attr.numeric = c("age", "capital_gain", "capital_loss", "hour_per_week")
  attr.ordinal = c("education_cat")
  data.nominal = data.original[,attr.nominal]
  data.numeric = data.original[,attr.numeric]
  data.ordinal = data.frame(data.original[,attr.ordinal])
  df.nominal = df[, attr.nominal]
  df.numeric = df[,attr.numeric]
  df.ordinal = data.frame(df[,attr.ordinal])
  #Creating output dataframe
  output <- data.frame(matrix(ncol = 2*k+1, nrow = nrow(df)))
  output.headings <- "Transaction ID"
  for( i in 1:k)
    output.headings <- append(output.headings, c(paste(i),paste(i,"-prox", sep="")), after = length(output.headings))
  colnames(output) <- output.headings
  
  #Normalizing numeric data
  data.numeric.normal = data.frame(scale(data.numeric))
  data.numeric.means = NULL
  for(i in 1:ncol(data.numeric)){
    data.numeric.means[i] <- mean(data.numeric[,i])  
  }
  data.numeric.sdev <- NULL
  for(i in 1:ncol(data.numeric)){
    data.numeric.sdev[i] <- sd(data.numeric[,i])
  }
  
  #Normalizing Ordinal Data
  data.ordinal.normal = data.frame(scale(data.ordinal))
  data.ordinal.means = NULL
  for(i in 1:ncol(data.ordinal)){
    data.ordinal.means[i] <- mean(data.ordinal[,i])  
  }
  data.ordinal.sdev <- NULL
  for(i in 1:ncol(data.ordinal)){
    data.ordinal.sdev[i] <- sd(data.ordinal[,i])
  }
  nominalDissimilarity = function(val1, val2){
    distance = 0
    for(i in 1:length(val1)){
      if(val1[i] != val2[i])
        distance = distance + 1
    }
    return(distance)
  }
  distances.nominal = vector(mode = "numeric", length = nrow(data.original))
  distances.numeric = vector(mode = "numeric", length = nrow(data.original))
  distances.ordinal = vector(mode = "numeric", length = nrow(data.original))
  for(i in 1:nrow(df)){
    record.nominal = data.frame(df.nominal[i,])
    record.numeric = data.frame((df.numeric[i,] - data.numeric.means)/data.numeric.sdev)
    record.ordinal = data.frame((df.ordinal[i,] - data.ordinal.means)/data.ordinal.sdev)
    for(j in 1:nrow(data.original)){
      distances.nominal[j] = nominalDissimilarity(record.nominal[i,],data.nominal[j,])
      distances.numeric[j] = 1-abs(sum(record.numeric[i,] * data.numeric.normal[j,]))/norm_vec(record.numeric[i,])/norm_vec(data.numeric.normal[j,])
      distances.ordinal[j] = sqrt(sum((record.ordinal[i,] - data.ordinal.normal[j,])^2))
    }
    distances.total = (ncol(data.nominal)*distances.nominal + ncol(data.numeric)*distances.numeric + ncol(data.ordinal)*distances.ordinal)/(ncol(data.nominal) + ncol(data.numeric) + ncol(data.ordinal))
    distances.total.sorted = sort(distances.total, index.return = TRUE)
    output[i,1] = i
    for(j in 1:k){
      output[i, 2*j] = distances.total.sorted$ix[j]
      output[i, 2*j+1] = distances.total.sorted$x[j]
    }
  }
  return(output)
}
#Creating sample test vectors
data.original = read.csv("~/projects/data-mining/hw1/income_tr.csv")
test = data.original[18,]

output = proximityIncomeEuclidean(4, test)
print("Proximity calculated using Euclidean Distances")
print(output)


output = proximityIncomeCosine(4, test)
print("Proximity calculated using Cosine Distances")
print(output)


