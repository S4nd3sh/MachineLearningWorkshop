# Force = Mass X Acceleration

set.seed(32)
force <-  data.frame(Mass = sample(1:100,size = 1000,replace = TRUE), Acceleration = sample(1:100,size = 1000,replace = TRUE))

force$Force <-  force$Mass * force$Acceleration

set.seed(43)
force$Force <-  force$Force + (rnorm(nrow(force)) * 100) #Introducing Gaussian noise

force.model1 <- lm(Force ~ Mass + Acceleration, data = force) #Creating Model 1 with Force = Mass + Acceleration
force.model2 <- lm(Force ~ Mass * Acceleration, data = force) #Creating Model 2 with Force = Mass X Acceleration

#Plotting the FIT
scatter.smooth(x=force$Mass / force$Acceleration, y=force$Force, main="Force ~ Mass + Acceleration") 
scatter.smooth(x=(force$Mass * force$Acceleration), y=force$Force, main="Force ~ Mass * Acceleration")

set.seed(23)
Force1.Pred <- predict(force.model1, force[1:20,])
Force2.Pred <- predict(force.model2, force[1:20,])


Result1 <- cbind(force[1:20,],Force1.Pred)
Result2 <- cbind(force[1:20,],Force2.Pred)

summary(force.model1)

summary(force.model2)

#Mean Absolute Error
MAE1 <- mean(abs(Result1$Force - Result1$Force1.Pred)) 
MAE2 <- mean(abs(Result2$Force - Result2$Force2.Pred))

#Mean Squared Error
MSE1 <- mean((Result1$Force - Result1$Force1.Pred)^2) 
MSE2 <- mean((Result2$Force - Result2$Force2.Pred)^2)

MAE1 <- round(MAE1, 2)
MAE2 <- round(MAE2, 2)

MSE1 <- round(MSE1, 2)
MSE2 <- round(MSE2, 2)

cat(" MAE1 = ", MAE1,"\t MSE1 = ",MSE1,"\n","MAE2 =  ",MAE2,"\t MSE2 =  ",MSE2)









