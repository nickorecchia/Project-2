##########Project Part 2############

library(ggplot2)
df<-read.csv('https://raw.githubusercontent.com/nickorecchia/Project-1/main/summary%20(cleaned).csv') 

#####scatterplots to visualize the correlations###
plot(df$PointsPerGame, df$MinutesPlayed)
plot(df$Steals, df$MinutesPlayed)
plot(df$Turnovers, df$MinutesPlayed)
plot(df$Rebounds, df$MinutesPlayed)

#INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF Points per Game
df$PointsPerGame2<-df$PointsPerGame^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$PointsPerGame3<-df$PointsPerGame^3#CUBIC TRANSFORMATION (3rd ORDER)
df$PointsPerGame4<-df$PointsPerGame^4

#TRANSFORMATIONS#
df$Steals2 <- df$Steals^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$Steals3 <- df$Steals^3#CUBIC TRANSFORMATION (3rd ORDER)

df$Turnovers2<-df$Turnovers^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$Turnovers3<-df$Turnovers^3#CUBIC TRANSFORMATION (3rd ORDER)

df$Rebounds2<-df$Rebounds^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$Rebounds3<-df$Rebounds^3#CUBIC TRANSFORMATION (3rd ORDER)

#A LOGARITHMIC TRANSFORMATION OF Points per Game
df$ln_PointsPerGame<-log(df$PointsPerGame)

#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model

#number of observations (rows) in the df
obs_count<-dim(df)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- df[train_ind, ] #pulls random rows for training
Testing <- df[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#PLOTTING THE TRAINING AND TESTING PARTITIONS
plot(MinutesPlayed ~ PointsPerGame, df, xlim=c(1.5,7), ylim=c(10,45)) #PLOT ENTIRE DATASET
plot(MinutesPlayed ~ PointsPerGame, Training, xlim=c(1.5,7), ylim=c(10,45), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(MinutesPlayed ~ PointsPerGame, Testing, xlim=c(1.5,7), ylim=c(10,45),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$PointsPerGame, Training$MinutesPlayed, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$PointsPerGame, Testing$MinutesPlayed, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

#BUILDING THE MODEL FROM THE TRAINING DATA
#####Model 1########
M1 <- lm(MinutesPlayed ~ PointsPerGame, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$MinutesPlayed)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$MinutesPlayed)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR  #####3.434382
RMSE_1_OUT #OUT-OF-SAMPLE ERROR     ####3.409132

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,30,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(PointsPerGame=x_grid))
plot(Training$MinutesPlayed ~ Training$PointsPerGame, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$MinutesPlayed ~ Testing$PointsPerGame, col='red', pch=3)

#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
######Model 2#######
M2 <- lm(MinutesPlayed ~ PointsPerGame + PointsPerGame2, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$MinutesPlayed)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$MinutesPlayed)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR    #####3.031583
RMSE_2_OUT #OUT-OF-SAMPLE ERROR    ####3.076115

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,30,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(PointsPerGame=x_grid, PointsPerGame2=x_grid^2))
plot(Training$MinutesPlayed ~ Training$PointsPerGame, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$MinutesPlayed ~ Testing$PointsPerGame, col='red', pch=3)

#BUILDING THE CUBIC MODEL FROM THE TRAINING DATA
######Model 3#####
M3 <- lm(MinutesPlayed ~ PointsPerGame + PointsPerGame2 + PointsPerGame3, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$MinutesPlayed)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$MinutesPlayed)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR   #####3.031579
RMSE_3_OUT #OUT-OF-SAMPLE ERROR    #####3.076112

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,30,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(PointsPerGame=x_grid, PointsPerGame2=x_grid^2, PointsPerGame3=x_grid^3))
plot(Training$MinutesPlayed ~ Training$PointsPerGame, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$MinutesPlayed ~ Testing$PointsPerGame, col='red', pch=3)


#BUILDING THE 4th order MODEL FROM THE TRAINING DATA
######4th order polynomial model######
M4 <- lm(MinutesPlayed ~ PointsPerGame + PointsPerGame2 + PointsPerGame3 + PointsPerGame4, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$MinutesPlayed)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$MinutesPlayed)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR   #####3.028291
RMSE_4_OUT #OUT-OF-SAMPLE ERROR    #####3.078572

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,30,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M4, list(PointsPerGame=x_grid, PointsPerGame2=x_grid^2, PointsPerGame3=x_grid^3,PointsPerGame4=x_grid^4))
plot(Training$MinutesPlayed ~ Training$PointsPerGame, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$MinutesPlayed ~ Testing$PointsPerGame, col='red', pch=3)


###########Part 2#######
########## PREDICTION MODELS #################################

#### MODEL 5 ####
#BUILDING FROM TRAINING DATA
AM1<- lm(MinutesPlayed ~ PointsPerGame + Steals + Rebounds, Training)
summary(AM1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
A_PRED_1_IN <- predict(AM1, Training) #generate predictions on the (in-sample) training data
View(A_PRED_1_IN)
View(AM1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
A_PRED_1_OUT <- predict(AM1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
A_RMSE_1_IN<-sqrt(sum((A_PRED_1_IN-Training$MinutesPlayed)^2)/length(A_PRED_1_IN))  #computes in-sample error
A_RMSE_1_OUT<-sqrt(sum((A_PRED_1_OUT-Testing$MinutesPlayed)^2)/length(A_PRED_1_OUT)) #computes out-of-sample 

A_RMSE_1_IN #IN-SAMPLE ERROR   ####2.746093                
A_RMSE_1_OUT #OUT-OF-SAMPLE ERROR    ####2.738222

##########################################################################################


#### MODEL 6 ####

AM2<- lm(MinutesPlayed ~ PointsPerGame + PointsPerGame2 + Steals + Steals2 + Rebounds + Rebounds2 , Training)
summary(AM2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
A_PRED_2_IN <- predict(AM2, Training) #generate predictions on the (in-sample) training data
View(A_PRED_2_IN)
View(AM2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
A_PRED_2_OUT <- predict(AM2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
A_RMSE_2_IN<-sqrt(sum((A_PRED_2_IN-Training$MinutesPlayed)^2)/length(A_PRED_2_IN))  #computes in-sample error
A_RMSE_2_OUT<-sqrt(sum((A_PRED_2_OUT-Testing$MinutesPlayed)^2)/length(A_PRED_2_OUT)) #computes out-of-sample 

A_RMSE_2_IN #IN-SAMPLE ERROR   #####2.400256              
A_RMSE_2_OUT #OUT-OF-SAMPLE ERROR   ####2.465767

##########################################################################################


#### MODEL 7 ####

AM3 <- lm(MinutesPlayed ~ PointsPerGame + PointsPerGame2 + PointsPerGame3 + Steals + Steals2 + Steals3 + Rebounds + Rebounds2 + Rebounds3 , Training)
summary(AM3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
A_PRED_3_IN <- predict(AM3, Training) #generate predictions on the (in-sample) training data
View(A_PRED_3_IN)
View(AM3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
A_PRED_3_OUT <- predict(AM3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
A_RMSE_3_IN<-sqrt(sum((A_PRED_3_IN-Training$MinutesPlayed)^2)/length(A_PRED_3_IN))  #computes in-sample error
A_RMSE_3_OUT<-sqrt(sum((A_PRED_3_OUT-Testing$MinutesPlayed)^2)/length(A_PRED_3_OUT)) #computes out-of-sample 

A_RMSE_3_IN #IN-SAMPLE ERROR  #####2.370386               
A_RMSE_3_OUT #OUT-OF-SAMPLE ERROR   ####2.469997

