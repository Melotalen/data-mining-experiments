norm_vec <- function(x) sqrt(sum(x^2))

irisProximityEuclidean = function(k, df){
  data <- read.csv("Iris.csv")
  data_col_names <- colnames(data)
  data <- data[, 1:length(data_col_names)-1]
  data.means <- NULL
  for(i in 1:ncol(data)){
    data.means[i] <- mean(data[,i])  
  }
  data.sdev <- NULL
  for(i in 1:ncol(data)){
    data.sdev[i] <- sd(data[,i])
  }
  data.scaled <- data.frame(scale(data))
  output <- data.frame(matrix(ncol = 2*k+1, nrow = nrow(df)))
  out.headings <- "Transaction ID"
  for( i in 1:k)
    out.headings <- append(out.headings, c(paste(i),paste(i,"-prox", sep="")), after = length(out.headings))
  colnames(output) <- out.headings
  for(j in 1:nrow(df)){
    val <- df[j,]
    val.scaled <- (val - data.means)/data.sdev
    dist.euclidean <- vector(mode = "numeric", length = nrow(data))
    for(i in 1:nrow(data)){
      dist.euclidean[i] = sum((data.scaled[i, ] - val.scaled)^2)
    }
    dist.sorted <- sort(dist.euclidean, index.return = TRUE)
    output[j,1] = j
    for(n in 1:k){
    output[j, 2*n] = dist.sorted$ix[n]
    output[j, 2*n+1] = dist.sorted$x[n]
    }
  }
  return(output)
  #plot(dist.sorted$x)
}

irisProximityCosine = function(k, df){
  data <- read.csv("Iris.csv")
  data_col_names <- colnames(data)
  data <- data[, 1:length(data_col_names)-1]
  data.means <- NULL
  for(i in 1:ncol(data)){
    data.means[i] <- mean(data[,i])  
  }
  data.sdev <- NULL
  for(i in 1:ncol(data)){
    data.sdev[i] <- sd(data[,i])
  }
  data.scaled <- data.frame(scale(data))
  output <- data.frame(matrix(ncol = 2*k+1, nrow = nrow(df)))
  out.headings <- "Transaction ID"
  for( i in 1:k)
    out.headings <- append(out.headings, c(paste(i),paste(i,"-prox", sep="")), after = length(out.headings))
  colnames(output) <- out.headings
  for(j in 1:nrow(df)){
    val <- df[j,]
    val.scaled <- (val - data.means)/data.sdev
    dist.cosine <- vector(mode = "numeric", length = nrow(data))
    for(i in 1:nrow(data)){
      dist.cosine[i] = 1 - abs(sum(data.scaled[i, ] * val.scaled)/norm_vec(data.scaled[i, ])/norm_vec(val.scaled))
    }
    dist.sorted <- sort(dist.cosine, index.return = TRUE)
    output[j,1] = j
    for(n in 1:k){
      output[j, 2*n] = dist.sorted$ix[n]
      output[j, 2*n+1] = dist.sorted$x[n]
    }
  }
  return(output)
  #plot(dist.sorted$x)
}

test = data.frame(c(5.1,1), c(3.5,2), c(1.4,1), c(0.2,1))
output = irisProximityEuclidean(4, test)
print(output)
output = irisProximityCosine(4, test)
print(output)
