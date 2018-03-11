source("K_NearestNeighbours.R")

#Input
iris <- datasets::iris

iris$Species <- as.character(iris$Species)

iris <- scale_values(iris,target = "Species")


#Split Dataset
set.seed(23)
index <- sample(1:nrow(iris),size = (0.7*nrow(iris)))

train <- iris[index,]
test <- iris[-index,]

rm(iris,index)

predicted <- knn(train = train,test = test,N = 5)

table(Actual = test$Class,Predicted = predicted)


######################################################################################################################
#Solve for diabetes dataset
diabetes <- read.csv("Pima Indians Diabetes Binary Classification dataset.csv",header = TRUE,stringsAsFactors = FALSE)

names(diabetes) <- LETTERS[1:ncol(diabetes)]

#Column I is your target variable





