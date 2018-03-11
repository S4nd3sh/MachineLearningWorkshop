#K Nearest Neighbours
scale_values <- function(input,target = "class"){
  options(stringsAsFactors = FALSE)
  
  columnNames <- names(input)
  
  target.class <- input[,target]
  
  input <- input[,-which(names(input) == target)]
  
  scale_fun <- function(x){
    x <- (x-min(x))/(max(x)-min(x))
  }
  input.list <- sapply(1:(length(input)),function(x) {scale_fun(input[x])})
  
  input.scaled <- data.frame(input.list,Class = target.class)
  
  #colnames(input.scaled) <- columnNames
  
  return(input.scaled)
}

knn <- function(train,test,N = 5){
  
  options(stringsAsFactors = FALSE)
  
  if(!require(dplyr)){
    install.packages("dplyr",dependencies = TRUE)
    library(dplyr)
  }
  
  class_prop <- train %>% group_by(Class) %>% summarize(Class_Prop = length(Class)/nrow(train))
  
  train.matrix <- as.matrix(train[,-which(names(train) == "Class")])
  test.matrix <- as.matrix(test[,-which(names(test) == "Class")])
  
  test.target <- as.character(test$Class)
  train.target <- as.character(train$Class)
  
  predicted.species <- vector("character",nrow(test.matrix))
  
  for(i in 1:nrow(test.matrix)){
    dist <- vector("numeric",nrow(train.matrix))
    
    for(j in 1:nrow(train.matrix)){
      #dist[j] <- mean(abs(train.matrix[j,]-test.matrix[i,]))
      dist[j] <- mean(sqrt((train.matrix[j,]-test.matrix[i,])**2))
    }
    
    target <- data.frame(index = 1:nrow(train.matrix), distances = dist)
    target <- target[order(target$distances),]
    
    temp <- as.data.frame(table(unlist(train[head(target$index,N),"Class"])))
    names(temp)[1] <- "Class"
    
    temp$Class <- as.character(temp$Class)
    
    temp <- inner_join(temp,class_prop,by = "Class")
    
    temp <- temp %>% group_by(Class) %>% summarize(Preference = Class_Prop/Freq)
    
    temp <- temp[order(temp$Preference,decreasing = F),]
    
    predicted.species[i] <- temp$Class[1]
    
  }
  return(predicted.species)
}
