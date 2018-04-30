#ReLU Function
ReLU <- function(z){
        max(0.01*z,z)
}

#ReLU Derivative
ReLU.derivative <- function(z){
        z[z>0] <- 1
        z[z<=0] <- 0.01
        return(z)
}

sigmoid <- function(z){
        1.0/(1.0+exp(-z))
}

sigmoid.derivative <- function(z){
        z*(1-z)
}

#Randomly Generates Weights
create.weights <- function(layers){
        weights <- vector("list",length(layers)-1)
        for(i in 1:length(weights)){
                weights[[i]] <- matrix(rnorm(layers[i]*layers[i+1]),nrow = layers[i],ncol = layers[i+1])
        }
        return(weights)
}

#Randomly Generates Biases
create.biases <- function(layers){
        biases <- vector("list",length(layers)-1)
        for(i in 1:length(biases)){
                biases[[i]] <- rnorm(layers[i+1])
        }
        return(biases)
}

#Feedforward / Forward Propogation
feedforward <- function(inp,weights,biases){
        layer <- vector("list",length(layers)-1)
        for(i in 1:(length(layers)-1)){
                if(i==(length(layers)-1)){

                        
                        layer[[i]] <- matrix(apply(layer[[i-1]],1,function(x){x %*% weights[[i]] + biases[[i]]}),nrow = nrow(layer[[i-1]]),byrow = TRUE)
                        
                        layer_dim  <- dim(layer[[i]])
                        layer[[i]] <- matrix(sigmoid(as.numeric(layer[[i]])),nrow = layer_dim[1],ncol = layer_dim[2],byrow = FALSE)
                }else if (i == 1){

                        
                        layer[[i]] <- matrix(apply(inp,1,function(x){x %*% weights[[i]] + biases[[i]]}),nrow = nrow(inp),byrow = TRUE)
                        
                        layer_dim  <- dim(layer[[i]])
                        layer[[i]] <- matrix(sigmoid(as.numeric(layer[[i]])),nrow = layer_dim[1],ncol = layer_dim[2],byrow = FALSE)
                        
                }else{
                        layer[[i]] <- matrix(apply(layer[[i-1]],1,function(x){x %*% weights[[i]] + biases[[i]]}),nrow = nrow(layer[[i-1]]),byrow = TRUE)
                        
                        layer_dim  <- dim(layer[[i]])
                        layer[[i]] <- matrix(ReLU(as.numeric(layer[[i]])),nrow = layer_dim[1],ncol = layer_dim[2],byrow = FALSE)
                        
                        
                }
        }
        return(layer)
}


#Calculating Delta Values
calculate_delta <- function(net_out,out,weights){
        Slope <- lapply(net_out,ReLU.derivative)
        
        Slope[[1]] <- sigmoid.derivative(net_out[[1]])
        Slope[[length(net_out)]] <- sigmoid.derivative(net_out[[length(net_out)]])
        delta <- vector("list",length(Slope))
        for(i in length(Slope):1){
                if(i == length(Slope)){
                        delta[[i]] <-  (out-net_out[[i]]) * Slope[[i]]
                }else{
                        Error_hidden <- delta[[i+1]] %*% t(weights[[i+1]])
                        delta[[i]] <- Error_hidden * Slope[[i]]
                }
        }
        return(delta)
}

#Updating Weights
updating_weights <- function(weights,inp,delta,net_out,step.size){
        for(i in length(weights):1){
                if(i != 1){
                        weights[[i]] <- weights[[i]] + t(net_out[[i-1]]) %*% delta[[i]] * step.size
                }else{
                        weights[[i]] <- weights[[i]] + t(inp) %*% delta[[i]] * step.size
                }
                
        }
        return(weights)
}

#Updating Biases
updating_biases <- function(biases,delta,step.size){
        for(i in 1:length(biases)){
                biases[[i]] <- biases[[i]] + colSums(delta[[i]]) * step.size
        }
        return(biases)
}


#Complete Neural Net operations (Forward and Backward Propogation)
Neural.Net <- function(layers = NULL,inp,out,epoch,step.size = 0.01,error.threshold = 0.01){
        weights <- create.weights(layers)
        biases <- create.biases(layers)
        
        error.vec <- vector("numeric",epoch)
        for(i in 1:epoch){
                ## Forward Propogation
                net_out <- feedforward(inp,weights,biases)
                ## Back Propogation
                delta <- calculate_delta(net_out,out,weights)
                weights <- updating_weights(weights,inp,delta,net_out,step.size)
                biases <- updating_biases(biases,delta,step.size)
                
                avg_error <- mean(abs(out - net_out[[length(net_out)]]))
                

                cat("\r                                                    \r")
                cat(paste("Epoch :",i," Avg_error = ",round(avg_error,10)))
                error.vec[i] <- avg_error
                

                
                if(avg_error <= error.threshold){
                        break
                }
        }
        return(list(weights = weights,biases = biases,Net_out = net_out,Error = error.vec))
}

Neural.Net.Predict <- function(Model,Test.inp){
        layer <- feedforward(inp = Test.inp,weights = Model$weights,biases = Model$biases)
        return(layer[length(layer)])
}





layers <- c(4,5,4)

cntr_inp <- as.matrix(rev(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1))))

cntr_out <- cntr_inp[nrow(cntr_inp):1,]

Output <- Neural.Net(layers = layers,inp = cntr_inp,out = cntr_out,epoch = 100000,
                     step.size = 0.8,error.threshold = 0.1)

Error <- Output$Error[Output$Error!=0]

plot(1:length(Error),Error,xlab="Epoch")
