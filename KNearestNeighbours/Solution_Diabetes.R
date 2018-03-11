#Solution for Diabetes dataset

diabetes <- read.csv("Pima Indians Diabetes Binary Classification dataset.csv",header = TRUE,stringsAsFactors = FALSE)

names(diabetes) <- LETTERS[1:ncol(diabetes)]

diabetes$I <- as.character(diabetes$I)

diabetes <- scale_values(diabetes,target = "I")


#Split Dataset
set.seed(23)
index <- sample(1:nrow(diabetes),size = (0.7*nrow(diabetes)))

train <- diabetes[index,]
test <- diabetes[-index,]

rm(diabetes,index)

predicted <- knn(train = train,test = test,N = 5)

table(Actual = test$Class,Predicted = predicted)