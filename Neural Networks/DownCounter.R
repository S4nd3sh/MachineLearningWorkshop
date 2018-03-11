
source("FF_Neural_Networks.R")

#Set layers for Neural Net
layers <- c(4,9,4)

#Counter Input
cntr_inp <- as.matrix(rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))))

#Counter Output
cntr_out <- cntr_inp[nrow(cntr_inp):1,]



#Invoke Neural Net function
set.seed(243)
Output <- Neural.Net(layers = layers,inp = cntr_inp,out = cntr_out,epoch = 100,
                     step.size = 0.8,error.threshold = 0.001)

#Predict for one state
cntr_inp[2,]
round(Neural.Net.Predict(Output,matrix( cntr_inp[2,] ,nrow = 1))[[1]])

#DownCount Prediction 
while(TRUE){
  for(i in 1:nrow(cntr_inp)){
    
    cat("Down Counter: ")
    cat("  \b\b")
    cat(strtoi(paste(round(unlist(Neural.Net.Predict(Output,matrix(cntr_inp[i,],nrow = 1)))),collapse = ""),base = 2))
    Sys.sleep(0.8)
    cat("\r")
    
    
  }
}

