#Load Packages
library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(randomForest)
library(car)
library(carData)
library(caret)

#Establish Data Frame
ydf=Yankees.Model.Train.Data

#Clean Data
ydf=subset(ydf,G>158) #Take out seasons that are not close to being full
str(ydf) 
summary(ydf)
colSums(is.na(ydf)) #Count Missing Values
ydf$xbh=ydf$X2B+ydf$X3B+ydf$HR #Add a new column for all combined XBHs

#Columns of interest
modcols=c("W","R","WHIP","ERA") 

#Split Data into training and test
set.seed(123)  
ydf$train <- ifelse(runif(nrow(ydf)) < 0.8, 1, 0)
train_data <- subset(ydf, train == 1)
test_data <- subset(ydf, train == 0)

#Create Models
lmyanks <- lm(W ~. , data = train_data[,modcols]) #linear regression model

rfyanks <- randomForest(W ~. , data = train_data[,modcols]) #random forest model

#Model Evaluations
lm_pred <- predict(lmyanks, newdata = test_data[,modcols]) #Predict on test data
rf_pred <- predict(rfyanks, newdata = test_data[,modcols])

lm_rmse <- sqrt(mean((test_data$W - lm_pred)^2)) #Calculate RMSE
rf_rmse <- sqrt(mean((test_data$W - rf_pred)^2))

print(paste("Linear Regression RMSE:", lm_rmse))
print(paste("Random Forest RMSE:", rf_rmse))

summary(lmyanks) #Summary of linear regression model, the chosen one

vif(lmyanks) #Check for multicollinearity

#Assumptions
plot(lmyanks, which=1, col=c("blue")) # residuals vs fitted 
plot(lmyanks, which=2, col=c("blue")) # qq plot for normality
plot(lmyanks, which=3, col=c("blue")) # for equal variance
plot(lmyanks, which=5, col=c("blue")) #Residual vs Leverage

#Predictions
yankswins=predict.lm(lmyanks,newdata = Yankees_Pitcher_Projections) 
print(yankswins) #87.25 predicted wins