# Loops

x <- 1:10                        #Vector Declaration

#For loop
for(i in 1:length(x)){
  cat("X = ",x[i],"\n")          #Concat
}


for(i in seq_along(x)){
  print(                         #Print
    paste("X = ",x[i],sep = "")  #Paste
  )
}


#While loop
x <- 0
while(x <= 5){
  print(x)
  x <- x+1
}

#IF statement
x <- 1
if(x == 1){
  print("X is equal to 1")
}

#IF-ELSE statement
x <- 10
if(x == 1){
  print("X is equal to 1")
}else{
  print("X is not equal to 1")
}

#IFELSE Statement vector
x <- 1:10
print(x)
ifelse(x<=5,TRUE,FALSE)


#Break statment
x <- 10
while(TRUE){
  print(x)
  if(x==0){
    break
  }
  x <- x-1
}


